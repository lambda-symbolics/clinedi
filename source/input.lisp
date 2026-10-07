;;;; -- Terminal input decoding --

(in-package #:clinedi)

(defparameter *keyboard-enhancement-enable-sequence*
  (format nil "~c[>4;1m~c[>5u" +escape-character+ +escape-character+)
  "Terminal controls requesting disambiguated keys and shifted alternate codes.")

(defparameter *keyboard-enhancement-disable-sequence*
  (format nil "~c[<u~c[>4;0m"
          +escape-character+
          +escape-character+)
  "Terminal controls restoring ordinary key reports after enhanced input.")

(defun enable-keyboard-enhancement (&key (stream *standard-output*))
  "Request distinguishable modified-key reports on STREAM.

Xterm modifyOtherKeys level one and Kitty disambiguation with shifted alternate
keys are requested together. Unsupported controls are ignored by conforming
terminals. Balance each call with DISABLE-KEYBOARD-ENHANCEMENT in the same screen
buffer."
  (write-string *keyboard-enhancement-enable-sequence* stream)
  (force-output stream)
  (values))

(defun disable-keyboard-enhancement (&key (stream *standard-output*))
  "Restore ordinary keyboard reporting on STREAM."
  (write-string *keyboard-enhancement-disable-sequence* stream)
  (force-output stream)
  (values))

(defparameter *bracketed-paste-enable-sequence*
  (format nil "~c[?2004h" +escape-character+)
  "The terminal control enabling bracketed paste mode.")

(defparameter *bracketed-paste-disable-sequence*
  (format nil "~c[?2004l" +escape-character+)
  "The terminal control disabling bracketed paste mode.")

(defun enable-bracketed-paste (&key (stream *standard-output*))
  "Enable bracketed paste mode on STREAM."
  (write-string *bracketed-paste-enable-sequence* stream)
  (force-output stream)
  (values))

(defun disable-bracketed-paste (&key (stream *standard-output*))
  "Disable bracketed paste mode on STREAM."
  (write-string *bracketed-paste-disable-sequence* stream)
  (force-output stream)
  (values))

(defparameter *bracketed-paste-end*
  (concatenate 'string (string +escape-character+) "[201~")
  "Terminal sequence ending a bracketed paste payload.")

(defun input--terminal-control-p (character)
  "True when CHARACTER is a C0, DEL or C1 terminal control."
  (let ((code (char-code character)))
    (or (< code 32) (<= 127 code 159))))

(defun input--read-bracketed-paste (stream)
  "Read a complete bracketed-paste payload from STREAM."
  (let ((payload (make-string-output-stream))
        (matched 0)
        (marker *bracketed-paste-end*))
    (loop for character = (read-char stream nil nil)
          do (cond ((null character)
                    (return))
                   ((char= character (char marker matched))
                    (incf matched)
                    (when (= matched (length marker))
                      (setf matched 0)
                      (return)))
                   (t
                    (when (plusp matched)
                      (write-string marker payload :end matched)
                      (setf matched 0))
                    (if (char= character (char marker 0))
                        (setf matched 1)
                        (write-char character payload)))))
    (when (plusp matched)
      (write-string marker payload :end matched))
    (sanitize-text (get-output-stream-string payload))))

(defun input--mouse-event (body)
  "Decode an SGR mouse report as (:SCROLL delta) or (:CLICK column row).

Wheel presses become scroll steps and a left-button press becomes a click at
its one-based column and row. Releases, motion, and other buttons are ignored
once the modifier bits are masked away."
  (handler-case
      (let* ((end (1- (length body)))
             (first-separator (position #\; body))
             (second-separator (and first-separator
                                    (position #\; body :start (1+ first-separator)))))
        (if (and (< 5 (length body) 64)
                 (char= (char body 0) #\<)
                 (char= (char body end) #\M)
                 first-separator second-separator)
            (let ((button (logand (parse-integer body :start 1 :end first-separator)
                                  (lognot #x1c)))
                  (column (parse-integer body :start (1+ first-separator)
                                              :end second-separator))
                  (row (parse-integer body :start (1+ second-separator) :end end)))
              (if (and (plusp column) (plusp row))
                  (case button
                    (#x40 (list :scroll -1))
                    (#x41 (list :scroll 1))
                    (0 (list :click column row))
                    (otherwise :ignore))
                  :ignore))
            :ignore))
    (error () :ignore)))

(defun input--split-field (text separator)
  "Split TEXT at SEPARATOR, preserving empty fields."
  (loop for start = 0 then (1+ end)
        for end = (position separator text :start start)
        collect (subseq text start end)
        while end))

(defun input--decimal-parameter (text)
  "Return TEXT's bounded unsigned decimal value, or NIL for invalid input."
  (when (and (plusp (length text)) (every #'digit-char-p text))
    (let ((value (parse-integer text)))
      (when (<= value #x10ffff)
        value))))

(defun input--key-fields (text)
  "Return numeric parameters and an optional Kitty shifted code from TEXT.

Validate the optional base-layout code without using it as inserted text.
Return NIL for malformed input or unrequested extra fields."
  (block nil
    (unless (<= 1 (length text) 64)
      (return nil))
    (let* ((parts (input--split-field text #\;))
           (keys (input--split-field (first parts) #\:))
           (code (input--decimal-parameter (first keys)))
           (shifted (and (second keys)
                         (input--decimal-parameter (second keys)))))
      (unless (and code (<= (length parts) 3) (<= (length keys) 3)
                   (case (length keys)
                     (1 t)
                     (2 shifted)
                     (3 (and (or shifted (zerop (length (second keys))))
                             (input--decimal-parameter (third keys))))))
        (return nil))
      (let ((parameters
              (cons code
                    (loop for part in (rest parts)
                          for index from 0
                          collect (if (and (zerop index) (zerop (length part)))
                                      1
                                      (input--decimal-parameter part))))))
        (when (every #'integerp parameters)
          (values parameters shifted))))))

(defun input--printable-code-p (code)
  "Return whether CODE is printable Unicode rather than a terminal control or key."
  (and (integerp code) (<= 32 code #x10ffff)
       (not (<= 127 code 159))
       (not (<= #xd800 code #xdfff))
       (not (<= #xe000 code #xf8ff))
       (not (null (code-char code)))))

(defun input--character-key-event (code modifiers &optional shifted)
  "Decode a Unicode CODE with normalized MODIFIERS and optional SHIFTED code."
  (cond
    ((or (null code) (logtest #x38 modifiers))
     :ignore)
    ((member code '(10 13))
     (if (zerop modifiers) :submit :insert-newline))
    ((= code 9)
     (case modifiers
       (0 :complete)
       (1 :complete-previous)
       (otherwise :ignore)))
    ((= code 27)
     (if (zerop modifiers) :escape :ignore))
    ((member code '(8 127))
     (if (logtest 6 modifiers) :kill-word :backspace))
    ((logbitp 2 modifiers)
     (case (if (<= 65 code 90) (+ code 32) code)
       (97 :home)
       (98 :left)
       (99 :interrupt)
       (100 :end-of-input)
       (101 :end)
       (102 :right)
       (104 :backspace)
       (105 :complete)
       ((106 109) :submit)
       (107 :kill-to-end)
       (108 :clear-screen)
       (110 :history-next)
       (112 :history-previous)
       (117 :kill-line)
       (119 :kill-word)
       (otherwise :ignore)))
    ((logbitp 1 modifiers)
     (case code
       ((66 98) :word-left)
       ((68 100) :kill-word-right)
       ((70 102) :word-right)
       (otherwise :ignore)))
    (t
     (let ((text-code (if (and (logbitp 0 modifiers) shifted) shifted code)))
       (if (input--printable-code-p text-code)
           (list :insert (string (code-char text-code)))
           :ignore)))))

(defun input--navigation-key-event (key modifiers)
  "Decode navigation KEY with normalized MODIFIERS."
  (if (logtest #x38 modifiers)
      :ignore
      (case key
        (:up :up)
        (:down :down)
        (:left (if (logtest 6 modifiers) :word-left :left))
        (:right (if (logtest 6 modifiers) :word-right :right))
        (:home (if (logbitp 2 modifiers) :scroll-top :home))
        (:end (if (logbitp 2 modifiers) :scroll-bottom :end))
        (:delete :delete)
        (:page-up (if (logbitp 2 modifiers) :previous-section :page-up))
        (:page-down (if (logbitp 2 modifiers) :next-section :page-down))
        (otherwise :ignore))))

(defun input--csi-key-event (body)
  "Decode numeric navigation, CSI-u, and modifyOtherKeys reports in BODY."
  (block nil
    (unless (plusp (length body))
      (return :ignore))
    (let* ((final (char body (1- (length body))))
           (prefix (subseq body 0 (1- (length body))))
           (navigation (case final
                         (#\A :up) (#\B :down) (#\C :right)
                         (#\D :left) (#\H :home) (#\F :end))))
      (multiple-value-bind (parameters shifted)
          (if (and navigation (zerop (length prefix)))
              (values '(1) nil)
              (input--key-fields prefix))
        (unless (and parameters
                     (or (char= final #\u) (not (find #\: prefix))))
          (return :ignore))
        (let ((code (first parameters))
              (modifier (or (second parameters) 1))
              (count (length parameters)))
          ;; Preserve the older CSI 5D shorthand for CSI 1;5D.
          (when (and navigation (= count 1))
            (setf modifier code code 1))
          (unless (<= 1 modifier 256)
            (return :ignore))
          (let ((modifiers (logand (1- modifier) #x3f)))
            (cond
              ((and navigation (= code 1) (<= count 2))
               (input--navigation-key-event navigation modifiers))
              ((and (char= final #\u) (<= count 2))
               (input--character-key-event code modifiers shifted))
              ((and (char= final #\~) (= code 27) (= count 3))
               (input--character-key-event (third parameters) modifiers))
              ((and (char= final #\~) (<= count 2))
               (if (member code '(10 13))
                   (input--character-key-event code modifiers)
                   (input--navigation-key-event
                    (case code
                      ((1 7) :home) (3 :delete) ((4 8) :end)
                      (5 :page-up) (6 :page-down))
                    modifiers)))
              (t
               :ignore))))))))

(defun input--csi-event (body stream)
  "Decode a CSI sequence BODY, reading paste payloads from STREAM."
  (cond
    ((string= body "Z")
     :complete-previous)
    ((and (plusp (length body)) (char= (char body 0) #\<))
     (input--mouse-event body))
    ((string= body "200~")
     (list :paste (input--read-bracketed-paste stream)))
    (t
     (input--csi-key-event body))))
(defun input--read-csi (stream)
  "Read and decode the body of one CSI sequence from STREAM."
  (let ((body (make-string-output-stream)))
    (loop for character = (read-char stream nil nil)
          do (cond ((null character)
                    (return :ignore))
                   (t
                    (write-char character body)
                    (let ((code (char-code character)))
                      (when (<= #x40 code #x7e)
                        (return
                          (input--csi-event
                           (get-output-stream-string body) stream)))))))))

(defun input--read-escape (stream escape-delay)
  "Read an escape sequence from STREAM, waiting ESCAPE-DELAY seconds."
  (let ((first (or (read-char-no-hang stream nil nil)
                   (progn
                     (when (plusp escape-delay)
                       (sleep escape-delay))
                     (read-char-no-hang stream nil nil)))))
    (case first
      ((nil) :escape)
      (#\[ (input--read-csi stream))
      (#\O
       (case (read-char stream nil nil)
         (#\A :up)
         (#\B :down)
         (#\C :right)
         (#\D :left)
         (#\H :home)
         (#\F :end)
         (t :ignore)))
      ((#\b #\B) :word-left)
      ((#\d #\D) :kill-word-right)
      ((#\f #\F) :word-right)
      ((#\backspace #\rubout) :kill-word)
      ((#\newline #\return) :insert-newline)
      (t :escape))))

(defun read-event (&key (stream *standard-input*) (escape-delay 0.002))
  "Read one semantic editing event from STREAM.

Printable input becomes (:INSERT text). Control and escape sequences become
editing keywords. Bracketed paste becomes one (:PASTE text) event, with terminal
controls sanitized before the text reaches an editor. CSI-u reports for Enter,
Tab, Shift-Tab, Escape, Backspace, and supported Ctrl editing keys map to their
semantic events. Legacy escape-prefix and CSI-u reports for Alt-B and Alt-F
move by words, Alt-D deletes the following word, and Alt-Backspace deletes the
preceding word. Modified Enter becomes :INSERT-NEWLINE when distinguishable.
Ctrl-Backspace and Ctrl-W become :KILL-WORD. Ctrl-Left and Ctrl-Right become
:WORD-LEFT and :WORD-RIGHT. When a line editor enables word-delimiter mode,
these events also recognize that editor's configured word delimiters. Arrow Up
and Down become :UP and :DOWN, while Ctrl-P and Ctrl-N retain explicit history
traversal. Shift-Tab becomes :COMPLETE-PREVIOUS. Ctrl-D becomes
:END-OF-INPUT; physical stream EOF becomes :STREAM-END."
  (let ((character (read-char stream nil nil)))
    (cond ((null character)
           :stream-end)
          ((char= character +escape-character+)
           (input--read-escape stream escape-delay))
          (t
           (case (char-code character)
             (1 :home)                 ; C-a
             (2 :left)                 ; C-b
             (3 :interrupt)            ; C-c
             (4 :end-of-input)         ; C-d
             (5 :end)                  ; C-e
             (6 :right)                ; C-f
             (8 :kill-word)            ; C-Backspace or C-h
             (9 :complete)             ; Tab
             ((10 13) :submit)
             (11 :kill-to-end)         ; C-k
             (12 :clear-screen)        ; C-l
             (14 :history-next)        ; C-n
             (16 :history-previous)    ; C-p
             (21 :kill-line)           ; C-u
             (23 :kill-word)           ; C-w
             (127 :backspace)
             (t
              (if (input--terminal-control-p character)
                  :ignore
                  (list :insert (string character)))))))))
