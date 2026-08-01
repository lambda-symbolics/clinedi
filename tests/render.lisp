;;;; -- Rendering tests --

(in-package #:clinedi/tests)

(defun run-render-tests ()
  "Run ANSI and wrapped-rendering regression tests."
  (let ((combined (format nil "e~c" (code-char #x301))))
    (check-equal "wide glyph ends at edge"
                 '(1 0 t)
                 (multiple-value-list
                  (screen-position "aa猫" :columns 4)))
    (check-equal "wide glyph wraps intact"
                 '(1 2 nil)
                 (multiple-value-list
                  (screen-position "aaa猫" :columns 4)))
    (check-equal "combining glyph advances one cell"
                 '(0 1 nil)
                 (multiple-value-list
                  (screen-position combined :columns 4))))
  (let ((cases `((""                    (0 2 nil))
                 ("a"                   (0 3 nil))
                 ("ab"                  (1 0 t))
                 (,(format nil "ab~%")   (1 0 nil))
                 (,(format nil "ab~%~%") (2 0 nil))
                 (,(format nil "abc~%")  (2 0 nil)))))
    (dolist (case cases)
      (destructuring-bind (text expected) case
        (check-equal (format nil "screen position for ~s" text)
                     expected
                     (multiple-value-list
                      (screen-position text
                                       :prompt-width 2
                                       :columns 4))))))
  (let ((text (format nil "a~%b")))
    (check-equal "cursor before newline"
                 '(0 1 nil)
                 (multiple-value-list
                  (screen-position text :columns 4 :end 1)))
    (check-equal "cursor after newline"
                 '(1 0 nil)
                 (multiple-value-list
                  (screen-position text :columns 4 :end 2)))
    (check-equal "display emits explicit carriage return"
                 (format nil "a~%~cb" #\return)
                 (with-output-to-string (stream)
                   (write-display text :stream stream))))
  (let ((text (format nil "zero~%one~%two~%three~%four")))
    (dolist (cursor (list 0 7 15 (length text)))
      (multiple-value-bind (start end window-cursor before-p after-p)
          (screen-window text :cursor cursor :columns 5 :rows 3)
        (declare (ignore before-p after-p))
        (check-equal (format nil "screen window preserves cursor ~d" cursor)
                     cursor
                     (+ start window-cursor))
        (check-true (format nil "screen window contains cursor ~d" cursor)
                    (<= start cursor end))
        (check-true (format nil "screen window fits row cap ~d" cursor)
                    (multiple-value-bind (row column pending-wrap)
                        (screen-position text
                                         :columns 5
                                         :start start
                                         :end end)
                      (declare (ignore column pending-wrap))
                      (<= (1+ row) 3))))))
  (let ((text "abcdefghijklmnop"))
    (multiple-value-bind (start end cursor before-p after-p)
        (screen-window text :cursor 8 :columns 4 :rows 2)
      (declare (ignore cursor before-p after-p))
      (check-true "wrapped screen window fits an exact-width row cap"
                  (multiple-value-bind (row column pending-wrap)
                      (screen-position text
                                       :columns 4
                                       :start start
                                       :end end)
                    (declare (ignore column pending-wrap))
                    (<= (1+ row) 2)))))
  (check-equal "completion columns use cell widths"
               (format nil "猫  a   ~%~c" #\return)
               (with-output-to-string (stream)
                 (print-candidates '("猫" "a")
                                   :columns 8
                                   :stream stream)))
  (multiple-value-bind (preamble prompt)
      (split-prompt (format nil "first~%second> "))
    (check-equal "prompt preamble" (format nil "first~%") preamble)
    (check-equal "editable prompt" "second> " prompt))
  (let ((layout (clinedi::screen--editor-layout "user" 7 10)))
    (check-equal "first input word moves past a short prompt row"
                 (format nil "~%user")
                 (clinedi::screen-editor-layout-display layout))
    (check-equal "wrapped first word begins at the next row"
                 '(0 1 0)
                 (first (clinedi::screen-editor-layout-positions layout))))
  (let ((layout (clinedi::screen--editor-layout "say user" 0 6)))
    (check-equal "input wraps before a whole word"
                 (format nil "say ~%user")
                 (clinedi::screen-editor-layout-display layout))
    (check-equal "cursor after a wrap separator starts the next row"
                 '(4 1 0)
                 (find 4
                       (clinedi::screen-editor-layout-positions layout)
                       :key #'first)))
  (multiple-value-bind (wrapped-text wrapped-display wrapped-cursor)
      (clinedi:wrap-styled-editor-text
       "lin"
       (ansi-colorize "lin" :green)
       :cursor 3
       :columns 10
       :prompt-width 7)
    (check-equal "a growing word may exactly fill its prompt row"
                 "lin"
                 wrapped-text)
    (check-equal "an exact-row editor cursor needs no display break"
                 3
                 wrapped-cursor)
    (check-equal "styled exact-row editor text preserves visible content"
                 wrapped-text
                 (ansi-strip wrapped-display)))
  (multiple-value-bind (wrapped-text wrapped-display wrapped-cursor)
      (clinedi:wrap-styled-editor-text
       "line"
       (ansi-colorize "line" :green)
       :cursor 4
       :columns 10
       :prompt-width 7)
    (check-equal "a growing editor word moves intact past its prompt"
                 (format nil "~%line")
                 wrapped-text)
    (check-equal "a reflowed editor cursor follows the complete word"
                 5
                 wrapped-cursor)
    (check-equal "styled editor reflow preserves visible content"
                 wrapped-text
                 (ansi-strip wrapped-display)))
  (let* ((text "abcdef")
         (layout (clinedi::screen--editor-layout text 2 4)))
    (check-equal "long input words still wrap by grapheme"
                 (format nil "~%abcdef")
                 (clinedi::screen-editor-layout-display layout))
    (check-equal "hard-wrapped long word retains source geometry"
                 '(4 2 0)
                 (find 4
                       (clinedi::screen-editor-layout-positions layout)
                       :key #'first)))
  (let* ((text "say user")
         (layout (clinedi::screen--editor-layout text 0 6))
         (presented
           (clinedi::render--insert-soft-breaks
            text
            (ansi-colorize text :red)
            (clinedi::screen-editor-layout-soft-breaks layout))))
    (check-equal "styled input preserves soft word breaks"
                 (clinedi::screen-editor-layout-display layout)
                 (ansi-strip presented)))
  (let* ((text "pri")
         (combined "printf")
         (accepted (clinedi::screen--editor-layout text 7 10))
         (with-suggestion
           (clinedi::screen--editor-layout
            combined 7 10 :stable-end (length text))))
    (check-equal "suggestion does not reflow accepted input"
                 '(3 1 0)
                 (find 3
                       (clinedi::screen-editor-layout-positions accepted)
                       :key #'first))
    (check-equal "continued suggestion keeps the accepted prefix in place"
                 "printf"
                 (clinedi::screen-editor-layout-display with-suggestion)))
  (let ((rendered
          (with-output-to-string (stream)
            (render-line "user"
                         :cursor 4
                         :prompt-width 7
                         :columns 10
                         :previous-row 0
                         :stream stream))))
    (check-true "live input renders its first word on the next row"
                (search (format nil "~a~%~cuser"
                                (ansi-clear-line-right)
                                #\return)
                        rendered)))
  (let* ((combined (format nil "e~c" (code-char #x301)))
         (rendered
           (with-output-to-string (stream)
             (render-line combined
                          :cursor 1
                          :columns 4
                          :stream stream))))
    (check-true "render cursor accepts an interior character index"
                (search combined rendered)))
  (let ((rendered
          (with-output-to-string (stream)
            (render-line "x" :columns 0 :stream stream))))
    (check-true "render normalizes a nonpositive terminal width"
                (search "x" rendered)))
  (let ((rendered
          (with-output-to-string (stream)
            (render-line "pri"
                         :cursor 3
                         :prompt-width 7
                         :columns 80
                         :previous-row 0
                         :suggestion "ntf example"
                         :stream stream))))
    (check-true "redraw starts by hiding cursor"
                (let ((hide (ansi-cursor-hide)))
                  (and (<= (length hide) (length rendered))
                       (string= hide rendered :end2 (length hide)))))
    (check-true "redraw restores cursor"
                (let ((show (ansi-cursor-show)))
                  (and (<= (length show) (length rendered))
                       (string= show rendered
                                :start2 (- (length rendered)
                                           (length show)))))))
  (let ((rendered
          (with-output-to-string (stream)
            (render-line "pri"
                         :cursor 3
                         :prompt-width 2
                         :columns 20
                         :previous-row 0
                         :footer-text "  print  printf"
                         :footer-display "  print  printf"
                         :stream stream))))
    (check-true "rendered footer follows editor text"
                (search "print  printf" rendered))
    (check-true "footer redraw returns to editor cursor"
                (search (ansi-cursor-up 1) rendered))
    (check-true "redraw overwrites content before clearing stale remainder"
                (< (search "print  printf" rendered)
                   (search (ansi-clear-below) rendered)))
    (check-true "redraw clears stale editor cells before its footer"
                (< (search (ansi-clear-line-right) rendered)
                   (search "print  printf" rendered))))
  (let ((*presentation-enabled* nil))
    (check-equal "disabled presentation leaves text plain"
                 "plain"
                 (ansi-colorize "plain" :red))
    (check-equal "disabled presentation leaves reverse video plain"
                 "plain"
                 (clinedi:ansi-reverse-video "plain")))
  (check-equal "basic coloring preserves its original wire format"
               (format nil "~c[1;31mred~c[0m" (code-char 27) (code-char 27))
               (ansi-colorize "red" :red :bold t))
  (check-equal "unknown colors fall back to white"
               (format nil "~c[37mplain~c[0m" (code-char 27) (code-char 27))
               (ansi-colorize "plain" :orange))
  (check-equal "indexed colors pass through the compatibility adapter"
               (format nil "~c[38;5;114mgreen~c[0m"
                       (code-char 27) (code-char 27))
               (ansi-colorize
                "green"
                (cl-colorist:indexed-color 114 :fallback :green)))
  (check-equal "reverse video preserves its original wire format"
               (format nil "~c[7mplain~c[0m" (code-char 27) (code-char 27))
               (clinedi:ansi-reverse-video "plain"))
  (check-equal "ANSI display width ignores styling"
               2
               (ansi-display-width (ansi-colorize "猫" :green)))
  (check-equal "ANSI strip removes OSC"
               "safe"
               (ansi-strip
                (format nil "~c]0;title~csafe" (code-char 27) (code-char 7))))
  (check-equal "ANSI strip removes C1 controls"
               "safe"
               (ansi-strip (format nil "~c31msafe" (code-char #x9b))))
  (values))
