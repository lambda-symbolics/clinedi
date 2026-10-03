;;;; -- Transcript viewport tests --

(in-package #:clinedi/tests)

(defun viewport-tests--rows (viewport)
  "Return every wrapped display row of VIEWPORT."
  (loop for index below (transcript-viewport-row-count viewport)
        collect (transcript-viewport-row-display viewport index)))

(defun viewport-tests--append (viewport text &rest regions)
  "Append unstyled TEXT with REGIONS to VIEWPORT."
  (transcript-viewport-append viewport text text :regions regions))

(defun run-viewport-tests ()
  "Run transcript viewport regression tests."
  (let ((viewport (make-transcript-viewport :width 6)))
    (viewport-tests--append viewport (format nil "abcdefghij~%"))
    (viewport-tests--append viewport (format nil "# one~%body~%"))
    (check-equal "chunks wrap to the width and a final newline adds no row"
                 '("abcdef" "ghij" "# one" "body")
                 (viewport-tests--rows viewport))
    (check-equal "an empty append is ignored"
                 4
                 (transcript-viewport-row-count
                  (viewport-tests--append viewport "")))
    (check-equal "following layout shows the newest rows and extra rows"
                 '(3 3 t)
                 (list (transcript-viewport-layout viewport 2 :extra-rows 1)
                       (transcript-viewport-maximum-top viewport)
                       (transcript-viewport-following-p viewport)))
    (transcript-viewport-scroll viewport -2)
    (check-equal "scrolling up leaves the tail"
                 '(1 nil)
                 (list (transcript-viewport-top viewport)
                       (transcript-viewport-following-p viewport)))
    (transcript-viewport-scroll viewport 10)
    (check-equal "scrolling past the end follows again"
                 t
                 (transcript-viewport-following-p viewport))
    (transcript-viewport-layout viewport 2)
    (transcript-viewport-scroll-to-top viewport)
    (transcript-viewport-scroll viewport 1)
    (check-equal "chunks and rows expose their plain text"
                 (list 2 (format nil "# one~%body~%") "ghij")
                 (list (transcript-viewport-chunk-count viewport)
                       (transcript-viewport-chunk-text viewport 1)
                       (transcript-viewport-row-text viewport 1)))
    (check-equal "the anchored second row stays first after reflow"
                 '(t 0 ("abcdefghij" "# one" "body"))
                 (list (transcript-viewport-resize viewport 12)
                       (transcript-viewport-top viewport)
                       (viewport-tests--rows viewport)))
    (check-equal "resizing to the same width changes nothing"
                 nil
                 (transcript-viewport-resize viewport 12))
    (transcript-viewport-follow viewport)
    (transcript-viewport-layout viewport 1)
    (flet ((header-p (text offset)
             (and (< offset (length text)) (char= (char text offset) #\#))))
      (check-equal "jumping up finds the previous accepted row"
                   '(t 1)
                   (list (transcript-viewport-jump viewport -1 #'header-p)
                         (transcript-viewport-top viewport)))
      (check-equal "jumping without a target leaves the viewport"
                   '(nil 1)
                   (list (transcript-viewport-jump viewport -1 #'header-p)
                         (transcript-viewport-top viewport))))
    (check-equal "page rows keep one row of context"
                 1
                 (transcript-viewport-page-rows viewport)))
  (let ((viewport (make-transcript-viewport :width 10))
        (text (format nil "猫 copy x~%")))
    (viewport-tests--append viewport text (list 2 6 '(:copy "source")))
    (transcript-viewport-layout viewport 3)
    (check-equal "a wide character occupies both of its cells"
                 (list nil text 0)
                 (multiple-value-list (transcript-viewport-hit viewport 2 1)))
    (check-equal "a click inside a region returns its action and character"
                 (list '(:copy "source") text 3)
                 (multiple-value-list (transcript-viewport-hit viewport 5 1)))
    (check-equal "clicks past the row, the rows, or the layout miss"
                 '(nil nil nil)
                 (list (transcript-viewport-hit viewport 10 1)
                       (transcript-viewport-hit viewport 1 2)
                       (transcript-viewport-hit viewport 1 4)))
    (let ((checkpoint (transcript-viewport-checkpoint viewport)))
      (viewport-tests--append viewport "more")
      (transcript-viewport-scroll-to-top viewport)
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback discards appends and scrolling"
                   '(1 t)
                   (list (transcript-viewport-row-count viewport)
                         (transcript-viewport-following-p viewport)))))
  (values))
