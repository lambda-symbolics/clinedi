(in-package #:clinedi/tests)

(defun test-assert (condition description)
  "Record one transferred terminal behavior check."
  (check-true description condition))

(defun test-input-decoder (stream &key (escape-delay 0.002))
  "Exercise a caller's literal-paste binding through the decoder callback."
  (let ((character (read-char stream nil nil)))
    (cond ((null character) ':stream-end)
          ((char= character (code-char 22))
           (clinedi:read-paste-burst stream :idle-seconds 0 :maximum-characters 1000000))
          (t (unread-char character stream)
             (clinedi:read-event :stream stream :escape-delay escape-delay)))))

(defclass test-buffered-terminal (clinedi:stream-terminal) ()
  (:default-initargs :event-decoder #'test-input-decoder
                    :event-prefix-p-function
                    (lambda (character) (find character (list #\Escape (code-char 22)))))
  (:documentation "A stream transport with a caller-supplied paste binding."))

(defclass protocol-recording-stream-terminal (stream-terminal)
  ((chunks
    :initform nil
    :accessor protocol-recording-stream-terminal-chunks
    :type list
    :documentation "Protocol writes captured in reverse order.")
   (write-count
    :initform 0
    :accessor protocol-recording-stream-terminal-write-count
    :type (integer 0)
    :documentation "The number of attempted protocol writes.")
   (fail-write-index
    :initarg :fail-write-index
    :initform nil
    :reader protocol-recording-stream-terminal-fail-write-index
    :type (or null integer)
    :documentation "The optional one-based write attempt that signals failure.")
   (flush-count
    :initform 0
    :accessor protocol-recording-stream-terminal-flush-count
    :type (integer 0)
    :documentation "The number of attempted protocol flushes."))
  (:documentation "A stream terminal recording and optionally failing protocol writes."))

(defmethod terminal-write
    ((terminal protocol-recording-stream-terminal) (text string))
  "Capture TEXT or signal the configured protocol-write failure."
  (let ((index (incf (protocol-recording-stream-terminal-write-count terminal))))
    (when (eql index
               (protocol-recording-stream-terminal-fail-write-index terminal))
      (error 'terminal-error
             :message "Injected protocol write failure."
             :operation ':write
             :cause nil))
    (push text (protocol-recording-stream-terminal-chunks terminal)))
  nil)

(defmethod terminal-flush ((terminal protocol-recording-stream-terminal))
  "Record one protocol flush without external output."
  (incf (protocol-recording-stream-terminal-flush-count terminal))
  nil)
(defun terminal-tests--contains-control-character-p (text)
  "Return true when TEXT contains an untrusted ESC or C1 control character."
  (loop for character across text
        for code = (char-code character)
        thereis (or (= code 27)
                    (<= 128 code 159))))
(defun test-terminal-input-decoding ()
  "Test production key decoding and bracketed or raw paste collection."
  (let* ((escape *terminal-escape-character*)
         (paste-start (format nil "~C[200~~" escape))
         (paste-end (format nil "~C[201~~" escape))
         (input
           (concatenate 'string
                        (format nil "~C[A" escape)
                        paste-start
                        "paste"
                        (format nil "~C[3J" escape)
                        paste-end
                        (string escape)
                        (string #\Return)
                        (format nil "~C[13;2u" escape)
                        (string (code-char 4))
                        (format nil "~C[100;5u" escape)))
         (terminal
           (make-instance 'test-buffered-terminal
                          :input-stream (make-string-input-stream input)
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :interactive-p t
                          :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :up)
                 "the production decoder recognizes an up arrow")
    (let ((paste-event (terminal-read-event terminal)))
      (test-assert (eq (first paste-event) :paste)
                   "the production decoder recognizes bracketed paste")
      (let ((editor (line-editor-create)))
        (line-editor-handle-event editor paste-event)
        (test-assert
         (not (terminal-tests--contains-control-character-p
               (line-editor-text editor)))
         "bracketed paste terminal controls are neutralized before display")))
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "legacy Alt-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "Clinedi enhanced Shift-Enter reaches the production decoder")
    (test-assert (eq (terminal-read-event terminal) :end-of-input)
                 "literal Ctrl-D requests end of input")
    (test-assert (eq (terminal-read-event terminal) :end-of-input)
                 "Clinedi enhanced Ctrl-D reaches the production decoder")
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "physical interactive stream EOF remains distinct from Ctrl-D"))
  (let ((terminal
          (make-instance 'test-buffered-terminal
                         :input-stream (make-string-input-stream "")
                         :output-stream (make-string-output-stream)
                         :input-file-descriptor 0
                         :interactive-p nil
                         :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "physical fallback stream EOF remains distinct from Ctrl-D"))
  (let* ((payload (format nil "first line~%second line"))
         (terminal
           (make-instance
            'test-buffered-terminal
            :input-stream
            (make-string-input-stream
             (concatenate 'string (string (code-char 22)) payload))
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) payload))
     "literal Ctrl-V paste bursts retain embedded newlines without submission"))
  (let* ((payload (format nil "first line~%second line"))
         (terminal
           (make-instance
            'test-buffered-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) payload))
     "an unbracketed multiline terminal burst becomes one paste event")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) payload))
       "an unbracketed multiline terminal paste never submits input")))
  (let* ((payload
           (format nil "first~%second~Cthird~C[A~C"
                   #\Tab *terminal-escape-character* (code-char 3)))
         (sanitized (sanitize-text payload))
         (terminal
           (make-instance
            'test-buffered-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) sanitized))
     "multiline paste precedence neutralizes later editing controls")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) sanitized))
       "multiline controls cannot submit or invoke Clinedi editing commands"))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "a multiline control paste remains one event"))
  #+sbcl
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input nil)
          (output nil))
      (unwind-protect
           (let ((payload (format nil "pipe first~%pipe second")))
             (setf input
                   (sb-sys:make-fd-stream
                    read-descriptor
                    :input t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil)
                   output
                   (sb-sys:make-fd-stream
                    write-descriptor
                    :output t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil))
             (write-string payload output)
             (finish-output output)
             (let* ((terminal
                      (make-instance
                       'test-buffered-terminal
                       :input-stream input
                       :output-stream (make-string-output-stream)
                       :input-file-descriptor read-descriptor
                       :interactive-p t
                       :columns 40))
                    (event (terminal-read-event terminal)))
               (test-assert
                (and (eq (first event) :paste)
                     (string= (second event) payload))
                "one OS-level multiline input write never becomes submission")
               (let ((single-line "pipe single-line paste"))
                 (write-string single-line output)
                 (finish-output output)
                 (test-assert
                  (equal (terminal-read-event terminal)
                         (list ':insert single-line))
                  "one OS-level single-line input write becomes one insert event"))))
        (when input
          (close input))
        (when output
          (close output))
        (ignore-errors (sb-posix:close read-descriptor))
        (ignore-errors (sb-posix:close write-descriptor)))))
  (let* ((payload "single-line paste")
         (terminal
           (make-instance
            'test-buffered-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (equal event (list ':insert payload))
     "a buffered plain-text burst becomes one insert event")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) payload))
       "a coalesced plain-text burst reaches Clinedi atomically"))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "a coalesced plain-text burst leaves no per-character events"))
  (let ((terminal
          (make-instance
           'test-buffered-terminal
           :input-stream
           (make-string-input-stream (format nil "a~C" #\Tab))
           :output-stream (make-string-output-stream)
           :input-file-descriptor 0
           :interactive-p t
           :columns 40)))
    (test-assert
     (equal (terminal-read-event terminal) '(:insert "a"))
     "a mixed buffered burst retains text before a control event")
    (test-assert (eq (terminal-read-event terminal) :complete)
                 "a mixed buffered burst retains its control event"))
  (dolist (case
           (list
            (list (string *terminal-escape-character*) ':escape)
            (list (string (code-char 3)) ':interrupt)
            (list (string #\Tab) ':complete)
            (list (string (code-char 127)) ':backspace)
            (list (format nil "~C[B" *terminal-escape-character*) ':down)
            (list (format nil "~C[C" *terminal-escape-character*) ':right)
            (list (format nil "~C[D" *terminal-escape-character*) ':left)))
    (destructuring-bind (input expected) case
      (let ((terminal
              (make-instance 'test-buffered-terminal
                             :input-stream (make-string-input-stream input)
                             :output-stream (make-string-output-stream)
                             :input-file-descriptor 0
                             :interactive-p t
                             :columns 40)))
        (test-assert (eq (terminal-read-event terminal) expected)
                     "ordinary raw and CSI keys retain semantic input events"))))
  (let* ((output (make-string-output-stream))
         (terminal
           (make-instance 'test-buffered-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream output
                          :input-file-descriptor 0))
         (expected
           (concatenate
            'string
            (terminal-keyboard-enhancement-enable-sequence)
            (terminal-bracketed-paste-enable-sequence)
            (terminal-bracketed-paste-disable-sequence)
            (terminal-keyboard-enhancement-disable-sequence))))
    (clinedi::terminal--enable-input-protocols terminal)
    (clinedi::terminal--disable-input-protocols terminal)
    (test-assert (string= (get-output-stream-string output) expected)
                 "terminal input protocols enable and disable once in order"))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 2))
         (failure nil))
    (handler-case
        (clinedi::terminal--enable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (let ((controls
            (format nil "~{~A~}"
                    (reverse
                     (protocol-recording-stream-terminal-chunks terminal)))))
      (test-assert failure
                   "partial protocol activation preserves its original failure")
      (test-assert
       (and (search (terminal-bracketed-paste-disable-sequence) controls)
            (search (terminal-keyboard-enhancement-disable-sequence) controls)
            (= (protocol-recording-stream-terminal-flush-count terminal) 1))
       "partial protocol activation attempts every cleanup control")))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 1))
         (failure nil))
    (handler-case
        (clinedi::terminal--disable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (test-assert failure
                 "protocol shutdown reports its first cleanup failure")
    (test-assert
     (and (equal (protocol-recording-stream-terminal-chunks terminal)
                 (list (terminal-keyboard-enhancement-disable-sequence)))
          (= (protocol-recording-stream-terminal-flush-count terminal) 1))
     "protocol shutdown continues after one cleanup write fails"))
  nil)

#+sbcl
(defun test-terminal-descriptor-tty-detection ()
  "Test TTY detection from the file descriptor rather than stream class."
  (let ((process nil)
        (terminal nil))
    (unwind-protect
         (progn
           (setf process
                 (sb-ext:run-program "/bin/sh"
                                     '("-c" "sleep 10")
                                     :pty t
                                     :wait nil))
           (let ((pty (sb-ext:process-pty process)))
             (setf terminal
                   (make-instance 'clinedi:posix-terminal
                    :input-stream (make-string-input-stream "")
                    :output-stream pty
                    :input-file-descriptor (sb-sys:fd-stream-fd pty)))
             (test-assert
              (not (interactive-stream-p
                    (stream-terminal-input-stream terminal)))
              "a wrapped input stream can be noninteractive while its descriptor is a TTY")
             (terminal-start terminal)
             (test-assert (terminal-interactive-p terminal)
                          "a TTY descriptor selects interactive mode")
             (terminal-stop terminal)
             (terminal-start terminal)
             (test-assert (terminal-interactive-p terminal)
                          "restarting preserves descriptor-based interactive mode")
             (terminal-stop terminal)))
      (when (and terminal (terminal-started-p terminal))
        (terminal-stop terminal))
      (when process
        (ignore-errors (sb-ext:process-kill process 15))
        (ignore-errors (sb-ext:process-wait process)))))
  nil)

(defun run-transport-tests ()
  "Run buffered input and optional native transport checks."
  (test-terminal-input-decoding)
  #+sbcl (test-terminal-descriptor-tty-detection)
  (run-transport-lifecycle-tests)
  t)


(defclass lifecycle-terminal (clinedi:stream-terminal)
  ((events :initform nil :accessor lifecycle-events :documentation "Lifecycle operations in reverse order.")
   (failure :initarg :failure :initform nil :accessor lifecycle-failure :documentation "Injected failing operation.")
   (writes :initform 0 :accessor lifecycle-writes :documentation "Attempted protocol writes."))
  (:default-initargs :input-stream (make-string-input-stream "")
                    :output-stream (make-string-output-stream)
                    :input-file-descriptor -1 :styling-p-function (lambda () t))
  (:documentation "A portable native-mode and output fault injector."))

(defun lifecycle-note (terminal event)
  "Record EVENT and signal its injected failure."
  (push event (lifecycle-events terminal))
  (when (equal event (lifecycle-failure terminal))
    (error "Injected lifecycle failure at ~S" event)))

(defmethod clinedi:terminal-capture-input-mode ((terminal lifecycle-terminal))
  (lifecycle-note terminal ':capture)
  ':saved)
(defmethod clinedi:terminal-activate-input-mode ((terminal lifecycle-terminal))
  (lifecycle-note terminal ':activate))
(defmethod clinedi:terminal-restore-input-mode ((terminal lifecycle-terminal) mode)
  (check-equal "restore exact mode snapshot" ':saved mode)
  (lifecycle-note terminal ':restore))
(defmethod clinedi:terminal-write ((terminal lifecycle-terminal) text)
  (declare (ignore text))
  (lifecycle-note terminal (incf (lifecycle-writes terminal))))
(defmethod clinedi:terminal-flush ((terminal lifecycle-terminal))
  (lifecycle-note terminal ':flush))

(defun run-transport-lifecycle-tests ()
  "Test successful, repeated and failing mode/protocol lifecycles."
  (dolist (failure '(nil :activate 1 2 :flush))
    (let ((terminal (make-instance 'lifecycle-terminal :failure failure))
          (caught nil))
      (handler-case (terminal-start terminal)
        (clinedi:terminal-error () (setf caught t)))
      (check-equal "activation failure propagates" (not (null failure)) caught)
      (if failure
          (progn
            (check-true "native rollback attempted" (member ':restore (lifecycle-events terminal)))
            (check-equal "failed activation clears lifecycle state" nil
                         (terminal-started-p terminal))
            (check-equal "failed activation clears styling state" nil
                         (terminal-styled-p terminal)))
          (progn
            (terminal-start terminal)
            (check-equal "repeated start is idempotent" 2 (lifecycle-writes terminal))
            (terminal-stop terminal)
            (check-equal "both protocols disabled" 4 (lifecycle-writes terminal))
            (let ((events (copy-list (lifecycle-events terminal))))
              (terminal-stop terminal)
              (check-equal "repeated stop is idempotent" events (lifecycle-events terminal)))))))
  (dolist (failure '(3 4 :flush :restore))
    (let ((terminal (make-instance 'lifecycle-terminal)) (caught nil))
      (terminal-start terminal)
      (setf (lifecycle-failure terminal) failure)
      (handler-case (terminal-stop terminal)
        (clinedi:terminal-error () (setf caught t)))
      (check-true "cleanup failure propagates" caught)
      (check-true "cleanup attempts native restore after output errors"
                  (member ':restore (lifecycle-events terminal)))
      (check-equal "cleanup always attempts every protocol" 4 (lifecycle-writes terminal))
      (check-equal "cleanup clears started state" nil (terminal-started-p terminal))
      (check-equal "cleanup clears saved native mode" nil
                   (stream-terminal-saved-terminal-mode terminal))))
  t)
