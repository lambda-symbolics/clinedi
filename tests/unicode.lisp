;;;; -- Unicode geometry tests --

(in-package #:clinedi/tests)

(defun unicode-tests--string (&rest codes)
  "Return a string containing the characters identified by CODES."
  (coerce (mapcar #'code-char codes) 'string))

(defun run-unicode-tests ()
  "Exercise grapheme boundaries, terminal widths, wrapping and sanitizing."
  (let* ((combined (unicode-tests--string #x65 #x301))
         (modified (unicode-tests--string #x1f44d #x1f3fd))
         (family (unicode-tests--string
                  #x1f468 #x200d #x1f469 #x200d #x1f467 #x200d #x1f466))
         (flag (unicode-tests--string #x1f1e8 #x1f1ff))
         (three-flags (unicode-tests--string #x1f1e8 #x1f1ff #x1f1f8))
         (heart (unicode-tests--string #x2764 #xfe0f))
         (keycap (unicode-tests--string #x31 #xfe0f #x20e3)))
    (check-equal "combining mark stays with its base"
                 (length combined)
                 (grapheme-next-boundary combined 0))
    (check-equal "emoji modifier stays with its base"
                 (length modified)
                 (grapheme-next-boundary modified 0))
    (check-equal "ZWJ family is one grapheme"
                 (length family)
                 (grapheme-next-boundary family 0))
    (check-equal "regional indicators form pairs"
                 2
                 (grapheme-next-boundary three-flags 0))
    (check-equal "third regional indicator starts another grapheme"
                 3
                 (grapheme-next-boundary three-flags 2))
    (check-equal "keycap components form one grapheme"
                 (length keycap)
                 (grapheme-next-boundary keycap 0))
    (check-equal "previous boundary finds a grapheme start"
                 0
                 (grapheme-previous-boundary combined 1))
    (check-equal "previous boundary crosses the complete grapheme"
                 0
                 (grapheme-previous-boundary combined (length combined)))
    (check-equal "boundary at or after skips the complete grapheme"
                 (length combined)
                 (grapheme-boundary-at-or-after combined 1))
    (check-equal "Hangul V does not attach to arbitrary text"
                 1
                 (grapheme-next-boundary
                  (unicode-tests--string #x20 #x1160) 0))
    (check-equal "Hangul T does not attach to arbitrary text"
                 1
                 (grapheme-next-boundary
                  (unicode-tests--string #x20 #x11a8) 0))
    (check-equal "regional indicators do not form emoji ZWJ sequences"
                 3
                 (grapheme-next-boundary
                  (unicode-tests--string #x1f1e6 #x1f1e7 #x200d #x1f1e8)
                  0))

    (check-equal "ASCII occupies one cell" 1 (text-cell-width "a"))
    (check-equal "CJK occupies two cells" 2 (text-cell-width "猫"))
    (check-equal "combining grapheme occupies one cell"
                 1 (text-cell-width combined))
    (check-equal "modified emoji occupies two cells"
                 2 (text-cell-width modified))
    (check-equal "joined emoji occupies two cells"
                 2 (text-cell-width family))
    (check-equal "flag pair occupies two cells"
                 2 (text-cell-width flag))
    (check-equal "emoji variation selector promotes the cluster"
                 2 (text-cell-width heart))
    (check-equal "keycap occupies two cells"
                 2 (grapheme-cell-width keycap 0 (length keycap)))

    (check-equal "cell prefix stops before a wide grapheme"
                 "a" (text-cell-prefix "a猫b" 2))
    (check-equal "cell prefix includes a fitting wide grapheme"
                 "a猫" (text-cell-prefix "a猫b" 3))
    (check-equal "cell prefix never splits combining graphemes"
                 combined
                 (text-cell-prefix
                  (concatenate 'string combined "x") 1))
    (check-equal "cell prefix never splits joined emoji"
                 "" (text-cell-prefix family 1))
    (check-equal "cell prefix excludes a partial requested slice"
                 "" (text-cell-prefix combined 1 :start 1 :end 1))
    (check-equal "cell window clips on grapheme boundaries"
                 '("‹bc" 3)
                 (multiple-value-list
                  (text-cell-window "a猫bc" 4 4)))
    (check-equal "cell window keeps the cursor on screen"
                 '("‹" 1)
                 (multiple-value-list
                  (text-cell-window "abc" 3 2)))

    (check-equal "wrapping prefers word boundaries"
                 '("one two" "three")
                 (wrap-text "one two three" 7))
    (check-equal "wrapping measures wide cells"
                 '("日本" "語")
                 (wrap-text "日本語" 4))
    (check-equal "wrapping preserves a combining grapheme"
                 (list combined "x")
                 (wrap-text (concatenate 'string combined "x") 1))
    (check-equal "oversize grapheme remains indivisible"
                 (list family)
                 (wrap-text family 1))
    (let* ((text "alpha beta gamma")
             (display
               (concatenate 'string
                            (ansi-colorize "alpha " :red)
                            (ansi-colorize "beta " :blue)
                            (ansi-colorize "gamma" :green)))
             (rows (wrap-styled-text text display 10)))
        (check-equal "styled wrapping prefers word boundaries"
                     '("alpha beta" "gamma")
                     (mapcar #'first rows))
        (check-true "styled wrapping preserves visible row content"
                    (every (lambda (row)
                             (string= (first row) (ansi-strip (second row))))
                           rows))
        (check-true "styled wrapping retains a continued ANSI style"
                    (search (ansi-colorize "gamma" :green) (second (second rows)))))
      (check-equal "wrapping preserves explicit empty lines"
                 '("a" "" "b" "")
                 (wrap-text (format nil "a~%~%b~%") 10)))

  (let* ((replacement (string (code-char #xfffd)))
         (hostile (format nil "a~cb~cc~cd"
                          (code-char 27)
                          (code-char 7)
                          (code-char #x9b))))
    (check-equal "sanitizing replaces terminal controls"
                 (concatenate 'string
                              "a" replacement "b" replacement "c"
                              replacement "d")
                 (sanitize-text hostile))
    (check-equal "sanitizing can remove terminal controls"
                 "abcd"
                 (sanitize-text hostile :replacement-character nil))
    (check-equal "sanitizing preserves normalized newlines"
                 (format nil "a~%b~%c")
                 (sanitize-text (format nil "a~cb~c~%c"
                                        #\return #\return)))
    (check-equal "single-line sanitizing replaces newlines with spaces"
                 "a b"
                 (sanitize-text (format nil "a~%b") :single-line-p t))
    (check-equal "sanitizing expands tabs deterministically"
                 "a  b"
                 (sanitize-text (format nil "a~cb" #\tab) :tab-width 2)))
  (values))
