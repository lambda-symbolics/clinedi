;;;; -- Terminal mode tracking tests --

(in-package #:clinedi/tests)

(defun mode-stream-tests--controls (text)
  "Return TEXT with each [ and ] preceded by an escape character."
  (with-output-to-string (stream)
    (loop for character across text
          do (when (member character '(#\[ #\]))
               (write-char (code-char 27) stream))
             (write-char character stream))))

(defun mode-stream-tests--feed (stream pieces)
  "Write each string of PIECES to STREAM as a separate packet."
  (dolist (piece pieces)
    (write-string piece stream)))

(defun run-mode-stream-tests ()
  "Run terminal mode tracking regression tests."
  (let* ((destination (make-string-output-stream))
         (stream (make-mode-tracking-output-stream destination))
         (enter (mode-stream-tests--controls
                 "[?1049h[2J[H[?1000h[?1006h[?25l[?7l")))
    (check-equal "restoring an untouched stream writes nothing"
                 '(nil "")
                 (list (mode-tracking-output-stream-restore stream)
                       (get-output-stream-string destination)))
    (mode-stream-tests--feed stream
                             (loop for character across enter
                                   collect (string character)))
    (check-equal "output passes through unchanged"
                 enter
                 (get-output-stream-string destination))
    (check-equal "controls split one character per packet are all tracked"
                 '(:alternate-screen :autowrap :cursor-visible
                   :mouse-reporting :sgr-mouse-encoding)
                 (sort (mode-tracking-output-stream-imposed-modes stream)
                       #'string<))
    (write-string (mode-stream-tests--controls "[?25h[?7h") stream)
    (check-equal "returning modes to their defaults releases them"
                 '(:alternate-screen :mouse-reporting :sgr-mouse-encoding)
                 (sort (mode-tracking-output-stream-imposed-modes stream)
                       #'string<))
    (get-output-stream-string destination)
    (check-equal "restoring reports the released modes"
                 '(:alternate-screen :mouse-reporting :sgr-mouse-encoding)
                 (sort (copy-list (mode-tracking-output-stream-restore stream))
                       #'string<))
    (check-equal "restoring resets attributes and leaves the alternate screen first"
                 (mode-stream-tests--controls "[0m[?1049l[?1000l[?1006l")
                 (get-output-stream-string destination))
    (check-equal "restoring twice writes nothing more"
                 '(nil "")
                 (list (mode-tracking-output-stream-restore stream)
                       (get-output-stream-string destination))))
  (let* ((destination (make-string-output-stream))
         (stream (make-mode-tracking-output-stream destination))
         (background (format nil "~a11;rgb:19/3C/B8~a"
                             (mode-stream-tests--controls "]")
                             (code-char 7))))
    (mode-stream-tests--feed stream (list (subseq background 0 3)
                                          (subseq background 3)))
    (check-equal "a split default color command is tracked"
                 '(:default-background)
                 (mode-tracking-output-stream-imposed-modes stream))
    (write-string (default-color-reset-sequence :background) stream)
    (check-equal "the matching reset releases the default color"
                 nil
                 (mode-tracking-output-stream-imposed-modes stream))
    (write-string (default-color-sequence :foreground
                                          (cl-colorist:rgb-color 1 2 3))
                  stream)
    (write-string (mode-stream-tests--controls "[?2004h") stream)
    (get-output-stream-string destination)
    (mode-tracking-output-stream-restore stream)
    (check-equal "restoring resets colors before attributes and modes"
                 (concatenate 'string
                              (default-color-reset-sequence :foreground)
                              (mode-stream-tests--controls "[0m[?2004l"))
                 (get-output-stream-string destination))
    (write-string (format nil "~a~a~a"
                          (semantic-prompt-marker-sequence :prompt-start)
                          (ansi-hyperlink "x" "https://example.com")
                          (mode-stream-tests--controls "[?1h[?12;1049;9999h"))
                  stream)
    (check-equal "untracked controls are ignored and combined parameters apply"
                 '(:alternate-screen)
                 (mode-tracking-output-stream-imposed-modes stream))
    (write-string (format nil "~a~a~a"
                          (mode-stream-tests--controls "[?")
                          (make-string 40 :initial-element #\1)
                          (mode-stream-tests--controls "[?1049l"))
                  stream)
    (check-equal "an overlong control is abandoned without hiding the next"
                 nil
                 (mode-tracking-output-stream-imposed-modes stream)))
  (values))
