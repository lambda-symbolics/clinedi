;;;; -- Terminal input decoding --

(in-package #:clinedi)

(defparameter *keyboard-enhancement-enable-sequence*
  (format nil "~c[>4;0m~c[>1u~c[>4;2m"
          +escape-character+
          +escape-character+
          +escape-character+)
  "Terminal controls enabling modified-key reports through supported protocols.")

(defparameter *keyboard-enhancement-disable-sequence*
  (format nil "~c[<u~c[>4;0m"
          +escape-character+
          +escape-character+)
  "Terminal controls restoring ordinary key reports after enhanced input.")

(defun enable-keyboard-enhancement (&key (stream *standard-output*))
  "Request distinguishable modified-key reports on STREAM.

Kitty keyboard disambiguation and xterm modifyOtherKeys level two are requested
together. Unsupported controls are ignored by conforming terminals. Calls must
be balanced with DISABLE-KEYBOARD-ENHANCEMENT while the same terminal is owned."
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

(defun input--csi-event (body stream)
  "Decode a CSI sequence BODY, reading paste payloads from STREAM."
  (cond ((string= body "A") :up)
        ((string= body "B") :down)
        ((string= body "Z") :complete-previous)
        ((member body '("1;5C" "5C") :test #'string=) :word-right)
        ((member body '("1;5D" "5D") :test #'string=) :word-left)
        ((string= body "C") :right)
        ((string= body "D") :left)
        ((string= body "H") :home)
        ((string= body "F") :end)
        ((member body '("1~" "7~") :test #'string=) :home)
        ((string= body "3~") :delete)
        ((member body '("4~" "8~") :test #'string=) :end)
        ((member body '("10u" "13u") :test #'string=)
         :submit)
        ((member body '("10;2u" "10;3u" "10;4u" "10;5u"
                        "10;6u" "10;7u" "10;8u"
                        "13;2u" "13;3u" "13;4u" "13;5u"
                        "13;6u" "13;7u" "13;8u"
                        "27;2;13~" "27;3;13~" "27;4;13~"
                        "27;5;13~" "27;6;13~" "27;7;13~"
                        "27;8;13~")
                 :test #'string=)
         :insert-newline)
        ((string= body "9u")
         :complete)
        ((string= body "9;2u")
         :complete-previous)
        ((string= body "27u")
         :escape)
        ((string= body "127u")
         :backspace)
        ((string= body "97;5u")
         :home)
        ((string= body "98;5u")
         :left)
        ((string= body "101;5u")
         :end)
        ((string= body "102;5u")
         :right)
        ((string= body "104;5u")
         :backspace)
        ((string= body "105;5u")
         :complete)
        ((member body '("106;5u" "109;5u") :test #'string=)
         :submit)
        ((string= body "107;5u")
         :kill-to-end)
        ((string= body "108;5u")
         :clear-screen)
        ((string= body "110;5u")
         :history-next)
        ((string= body "112;5u")
         :history-previous)
        ((string= body "117;5u")
         :kill-line)
        ((string= body "119;5u")
         :kill-word)
        ((string= body "98;3u")
         :word-left)
        ((string= body "100;3u")
         :kill-word-right)
        ((string= body "102;3u")
         :word-right)
        ((string= body "127;3u")
         :kill-word)
        ((member body '("100;5u" "27;5;100~") :test #'string=)
         :end-of-input)
        ((member body '("8;5u" "127;5u"
                        "27;5;8~" "27;5;127~")
                 :test #'string=)
         :kill-word)
        ((member body '("99;5u" "27;5;99~")
                 :test #'string=)
         :interrupt)
        ((string= body "200~")
         (list :paste (input--read-bracketed-paste stream)))
        (t :ignore)))

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
         (#\H :home)
         (#\F :end)
         (t :ignore)))
      ((#\newline #\return) :insert-newline)
      (t :escape))))

(defun read-event (&key (stream *standard-input*) (escape-delay 0.002))
  "Read one semantic editing event from STREAM.

Printable input becomes (:INSERT text). Control and escape sequences become
editing keywords. Bracketed paste becomes one (:PASTE text) event, with terminal
controls sanitized before the text reaches an editor. CSI-u reports for Enter,
Tab, Shift-Tab, Escape, Backspace, and supported Ctrl editing keys map to their
semantic events. Alt-B and Alt-F move by words, Alt-D deletes the following
word, and Alt-Backspace deletes the preceding word. Modified Enter becomes
:INSERT-NEWLINE when distinguishable.
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
