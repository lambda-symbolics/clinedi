(in-package #:clinedi)

;;;; -- Stream transport and input protocol --

(defun terminal--default-event-decoder (stream &key (escape-delay 0.002))
  "Decode an event from STREAM with Clinedi's default key policy."
  (read-event :stream stream :escape-delay escape-delay))

(defun terminal--default-styling-p ()
  "Return whether the current environment permits styling."
  (not (eq (cl-colorist:effective-color-level) ':none)))


;;;; -- Terminal Defaults --

(defparameter *terminal-default-columns* 80
  "The fallback terminal width when no positive width is supplied.")

(defparameter *terminal-default-rows* 24
  "The fallback terminal height when no positive height is supplied.")

(defparameter *terminal-escape-character* (code-char 27)
  "The ASCII escape character used by trusted terminal controls.")
(defun terminal-bracketed-paste-enable-sequence ()
  "Return Clinedi's trusted bracketed-paste enable control."
  (with-output-to-string (stream)
    (enable-bracketed-paste :stream stream)))
(defun terminal-bracketed-paste-disable-sequence ()
  "Return Clinedi's trusted bracketed-paste disable control."
  (with-output-to-string (stream)
    (disable-bracketed-paste :stream stream)))
(defun terminal-keyboard-enhancement-enable-sequence ()
  "Return Clinedi's trusted keyboard-enhancement enable controls."
  (with-output-to-string (stream)
    (enable-keyboard-enhancement :stream stream)))
(defun terminal-keyboard-enhancement-disable-sequence ()
  "Return Clinedi's trusted keyboard-enhancement disable controls."
  (with-output-to-string (stream)
    (disable-keyboard-enhancement :stream stream)))

(defclass terminal ()
  ((rows
    :initarg :rows
    :initform *terminal-default-rows*
    :accessor terminal-rows
    :type (integer 1)
    :documentation "The current terminal height in character cells.")
   (columns
    :initarg :columns
    :initform *terminal-default-columns*
    :accessor terminal-columns
    :type (integer 1)
    :documentation "The current terminal width in character cells.")
   (interactive-p
    :initarg :interactive-p
    :initform nil
    :accessor terminal-interactive-p
    :type boolean
    :documentation "Whether this terminal currently accepts noncanonical input.")
   (styled-p
    :initarg :styled-p
    :initform nil
    :accessor terminal-styled-p
    :type boolean
    :documentation "Whether trusted output may include color and emphasis controls.")
   (started-p
    :initform nil
    :accessor terminal-started-p
    :type boolean
    :documentation "Whether this terminal has entered its active lifecycle."))
  (:documentation "A replaceable primary-screen terminal transport."))

(defclass stream-terminal (terminal)
  ((event-prefix-p-function
    :initarg :event-prefix-p-function
    :initform (lambda (character) (char= character #\Escape))
    :reader stream-terminal-event-prefix-p-function
    :documentation "Predicate recognizing a caller's event prefix before multiline paste classification.")
   (event-decoder
    :initarg :event-decoder
    :initform #'terminal--default-event-decoder
    :reader stream-terminal-event-decoder
    :documentation "Application event-decoding callback taking STREAM and :ESCAPE-DELAY.")
   (styling-p-function
    :initarg :styling-p-function
    :initform #'terminal--default-styling-p
    :reader stream-terminal-styling-p-function
    :documentation "Policy callback deciding whether interactive output may be styled.")
   (input-stream
    :initarg :input-stream
    :reader stream-terminal-input-stream
    :type stream
    :documentation "The character stream carrying terminal input.")
   (pending-input-stream
    :initform nil
    :accessor stream-terminal-pending-input-stream
    :type (or null stream)
    :documentation "Buffered terminal bytes awaiting semantic event decoding.")
   (output-stream
    :initarg :output-stream
    :reader stream-terminal-output-stream
    :type stream
    :documentation "The character stream receiving terminal output.")
   (input-file-descriptor
    :initarg :input-file-descriptor
    :reader stream-terminal-input-file-descriptor
    :type integer
    :documentation "The POSIX descriptor whose termios state is controlled.")
   (saved-terminal-mode
    :initform nil
    :accessor stream-terminal-saved-terminal-mode
    :type t
    :documentation "The exact termios value restored when the terminal stops."))
  (:documentation "A buffered character-stream terminal with optional native mode operations."))
(defun terminal-set-dimensions (terminal columns &key rows)
  "Set TERMINAL's positive cell dimensions through the canonical writer."
  (setf (terminal-columns terminal) (max 1 columns))
  (when rows
    (setf (terminal-rows terminal) (max 1 rows)))
  terminal)

(defgeneric terminal-start (terminal)
  (:documentation "Start TERMINAL without entering an alternate screen."))

(defgeneric terminal-stop (terminal)
  (:documentation "Restore TERMINAL input state and finish its lifecycle."))

(defgeneric terminal-read-event (terminal)
  (:documentation "Read and return one semantic input event from TERMINAL."))
(defgeneric terminal-input-ready-p (terminal)
  (:documentation "Return true when TERMINAL can read an event without blocking."))

(defmethod terminal-input-ready-p ((terminal terminal))
  "Assume application-provided TERMINAL transports have an event ready."
  (declare (ignore terminal))
  t)

(defgeneric terminal-flush (terminal)
  (:documentation "Make all pending TERMINAL output visible."))

(defgeneric terminal-write (terminal text)
  (:documentation "Write trusted presentation through TERMINAL."))

(defmethod terminal-write ((terminal stream-terminal) (text string))
  "Write trusted TEXT to TERMINAL's output stream."
  (write-string text (stream-terminal-output-stream terminal))
  nil)

(defmethod terminal-flush ((terminal stream-terminal))
  "Flush TERMINAL's output stream."
  (finish-output (stream-terminal-output-stream terminal))
  nil)
(defun terminal--disable-input-protocols (terminal)
  "Best-effort restore ordinary keyboard reporting and paste handling."
  (let ((failure nil))
    (labels ((attempt (function)
               "Run FUNCTION, retaining only the first signaled failure."
               (handler-case
                   (funcall function)
                 (error (condition)
                   (unless failure
                     (setf failure condition))))))
      (attempt
       (lambda ()
         (terminal-write terminal
                          (terminal-bracketed-paste-disable-sequence))))
      (attempt
       (lambda ()
         (terminal-write terminal
                          (terminal-keyboard-enhancement-disable-sequence))))
      (attempt (lambda () (terminal-flush terminal))))
    (when failure
      (error failure)))
  nil)
(defun terminal--enable-input-protocols (terminal)
  "Enable modified keys and bracketed paste, rolling back partial output."
  (handler-case
      (progn
        (terminal-write terminal
                         (terminal-keyboard-enhancement-enable-sequence))
        (terminal-write terminal (terminal-bracketed-paste-enable-sequence))
        (terminal-flush terminal))
    (error (condition)
      (ignore-errors (terminal--disable-input-protocols terminal))
      (error condition)))
  nil)

(defmethod terminal-input-ready-p ((terminal stream-terminal))
  "Return true when TERMINAL input can be consumed without blocking."
  (let ((pending (stream-terminal-pending-input-stream terminal)))
    (when (and pending (not (listen pending)))
      (setf (stream-terminal-pending-input-stream terminal) nil
            pending nil))
    (not
     (null
      (or (not (terminal-interactive-p terminal))
          pending
          (listen (stream-terminal-input-stream terminal)))))))

(defmethod terminal-read-event ((terminal stream-terminal))
  "Read one key, escape sequence, paste, fallback line, or physical stream end."
  (if (terminal-interactive-p terminal)
      (terminal-read-editing-event terminal)
      (let ((line (read-line (stream-terminal-input-stream terminal) nil nil)))
        (if line
            (list :line line)
            :stream-end))))
(defun stream-terminal-create
    (&key
       (input-stream *standard-input*)
       (output-stream *standard-output*)
       (input-file-descriptor 0)
       (event-prefix-p-function (lambda (character) (char= character #\Escape)))
       (event-decoder #'terminal--default-event-decoder)
     (styling-p-function #'terminal--default-styling-p)
     (rows *terminal-default-rows*)
       (columns *terminal-default-columns*))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :event-prefix-p-function event-prefix-p-function
                 :event-decoder event-decoder
                 :styling-p-function styling-p-function
                 :input-stream input-stream
                 :output-stream output-stream
                 :input-file-descriptor input-file-descriptor
                 :rows (if (plusp rows)
                           rows
                           *terminal-default-rows*)
                 :columns (if (plusp columns)
                              columns
                              *terminal-default-columns*)))

;;;; -- Enhanced Terminal Input --

(defparameter *terminal-escape-delay-seconds* 0.002
  "The seconds allowed for bytes following one terminal Escape character.")

(defparameter *terminal-unbracketed-paste-coalesce-seconds* 0.002
  "The short delay used to collect one unbracketed terminal input burst.")

(defparameter *terminal-unbracketed-paste-maximum-characters* 1000000
  "The maximum characters retained from one unbracketed paste burst.")
(defun terminal--read-input-burst (stream)
  "Return one bounded burst currently available from STREAM, or NIL at EOF."
  (let ((first (read-char stream nil nil)))
    (unless first
      (return-from terminal--read-input-burst nil))
    (when (plusp *terminal-unbracketed-paste-coalesce-seconds*)
      (sleep *terminal-unbracketed-paste-coalesce-seconds*))
    (let ((characters (list first))
          (count 1))
      (loop while (< count *terminal-unbracketed-paste-maximum-characters*)
            for character = (read-char-no-hang stream nil nil)
            while character
            do (push character characters)
               (incf count))
      (coerce (nreverse characters) 'string))))
(defun terminal--multiline-paste-burst-p (text event-prefix-p-function)
  "Return true when TEXT is one unbracketed burst containing a line break.

This classification takes precedence over controls later in the burst so pasted
text cannot invoke editing commands."
  (and (> (length text) 1)
       (not (funcall event-prefix-p-function (char text 0)))
       (or (find #\Newline text)
           (find #\Return text))
       t))
(defun terminal--plain-text-burst-p (text)
  "Return true when TEXT is one multi-character burst without terminal controls."
  (and (> (length text) 1)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (not (or (< code 32)
                           (<= 127 code 159)))))
              text)
       t))
(defun terminal--decode-buffered-editing-event
    (terminal prefix-stream
     &key (escape-delay *terminal-escape-delay-seconds*))
  "Decode one event from PREFIX-STREAM followed by TERMINAL's raw input."
  (let ((event
          (funcall (stream-terminal-event-decoder terminal)
           (make-concatenated-stream
            prefix-stream
            (stream-terminal-input-stream terminal))
           :escape-delay escape-delay)))
    (setf (stream-terminal-pending-input-stream terminal)
          (and (listen prefix-stream) prefix-stream))
    event))
(defun terminal-read-editing-event
    (terminal &key (escape-delay *terminal-escape-delay-seconds*))
  "Read one event, batching plain input and preserving multiline paste bursts."
  (let ((pending (stream-terminal-pending-input-stream terminal)))
    (when (and pending (not (listen pending)))
      (setf (stream-terminal-pending-input-stream terminal) nil
            pending nil))
    (if pending
        (terminal--decode-buffered-editing-event
         terminal pending :escape-delay escape-delay)
        (let ((burst
                (terminal--read-input-burst
                 (stream-terminal-input-stream terminal))))
          (cond
            ((null burst)
             ':stream-end)
            ((terminal--multiline-paste-burst-p
              burst (stream-terminal-event-prefix-p-function terminal))
             (list ':paste (sanitize-text burst)))
            ((terminal--plain-text-burst-p burst)
             (list ':insert burst))
            (t
             (terminal--decode-buffered-editing-event
              terminal
              (make-string-input-stream burst)
              :escape-delay escape-delay)))))))


(define-condition terminal-error (error)
  ((message :initarg :message :reader terminal-error-message
            :documentation "Description of the failed terminal operation.")
   (operation :initarg :operation :reader terminal-error-operation
              :documentation "Operation that failed.")
   (cause :initarg :cause :initform nil :reader terminal-error-cause
          :documentation "Underlying condition, when available."))
  (:report (lambda (condition stream) (write-string (terminal-error-message condition) stream)))
  (:documentation "A terminal mode, input or output failure."))

(defgeneric terminal-capture-input-mode (terminal)
  (:documentation "Capture the current native input mode, or NIL for a non-TTY."))
(defgeneric terminal-activate-input-mode (terminal)
  (:documentation "Enter noncanonical no-echo input mode."))
(defgeneric terminal-restore-input-mode (terminal mode)
  (:documentation "Restore an exact previously captured native input MODE."))

(defmethod terminal-capture-input-mode ((terminal stream-terminal))
  (declare (ignore terminal))
  nil)
(defmethod terminal-activate-input-mode ((terminal stream-terminal))
  (declare (ignore terminal))
  nil)
(defmethod terminal-restore-input-mode ((terminal stream-terminal) mode)
  (declare (ignore terminal mode))
  nil)

(defmethod terminal-start ((terminal stream-terminal))
  "Enter interactive mode, rolling back both protocols and native mode on failure."
  (unless (terminal-started-p terminal)
    (let ((saved-mode (terminal-capture-input-mode terminal)))
      (if (null saved-mode)
          (setf (terminal-interactive-p terminal) nil
                (terminal-styled-p terminal) nil
                (terminal-started-p terminal) t)
          (handler-case
              (progn
                (terminal-activate-input-mode terminal)
                (setf (stream-terminal-saved-terminal-mode terminal) saved-mode
                      (terminal-interactive-p terminal) t
                      (terminal-styled-p terminal)
                      (funcall (stream-terminal-styling-p-function terminal))
                      (terminal-started-p terminal) t)
                (terminal--enable-input-protocols terminal))
            (error (condition)
              (ignore-errors (terminal-restore-input-mode terminal saved-mode))
              (setf (stream-terminal-saved-terminal-mode terminal) nil
                    (terminal-interactive-p terminal) nil
                    (terminal-styled-p terminal) nil
                    (terminal-started-p terminal) nil)
              (error 'terminal-error :message "Could not enter terminal input mode."
                     :operation ':start :cause condition))))))
  terminal)

(defmethod terminal-stop ((terminal stream-terminal))
  "Stop all input protocols and restore native mode, attempting every cleanup."
  (when (terminal-started-p terminal)
    (let ((failure nil)
          (saved-mode (stream-terminal-saved-terminal-mode terminal)))
      (flet ((attempt (function)
               (handler-case (funcall function)
                 (error (condition) (unless failure (setf failure condition))))))
        (when (terminal-interactive-p terminal)
          (attempt (lambda () (terminal--disable-input-protocols terminal))))
        (when saved-mode
          (attempt (lambda () (terminal-restore-input-mode terminal saved-mode)))))
      (setf (stream-terminal-saved-terminal-mode terminal) nil
            (terminal-interactive-p terminal) nil
            (terminal-styled-p terminal) nil
            (terminal-started-p terminal) nil)
      (when failure
        (error 'terminal-error :message "Could not completely restore terminal state."
               :operation ':stop :cause failure))))
  terminal)

(defun read-paste-burst (stream &key (idle-seconds 0.05) (maximum-characters 1000000))
  "Read a bounded literal paste until an idle gap, returning :PASTE or :IGNORE."
  (let ((characters nil))
    (loop repeat maximum-characters
          for character = (or (read-char-no-hang stream nil nil)
                              (progn (sleep idle-seconds)
                                     (read-char-no-hang stream nil nil)))
          while character do (push character characters))
    (if characters
        (list ':paste (coerce (nreverse characters) 'string))
        ':ignore)))
