;;;; -- Fullscreen frame painting tests --

(in-package #:clinedi/tests)

(defun frame-painter-tests--paint (painter rows &rest arguments)
  "Paint ROWS through PAINTER and return the control string it wrote."
  (let ((written nil))
    (apply #'frame-painter-paint painter rows
           (lambda (controls) (setf written controls))
           arguments)
    written))

(defun frame-painter-tests--painted-rows (controls)
  "Return the one-based screen rows CONTROLS rewrites, in order."
  (loop with marker = (format nil "~c[2K" (code-char 27))
        for start = (search marker controls) then (search marker controls :start2 (1+ start))
        while start
        collect (let* ((position-end (position #\H controls :end start :from-end t))
                       (position-start (position #\[ controls :end position-end :from-end t)))
                  (parse-integer controls :start (1+ position-start)
                                          :end (position #\; controls :start position-start)))))

(defun run-frame-painter-tests ()
  "Run fullscreen frame painting regression tests."
  (let ((painter (make-frame-painter))
        (escape (code-char 27)))
    (let ((controls (frame-painter-tests--paint painter '("one" "two" "three" "four")
                                                :height 3 :width 10
                                                :cursor-row 7 :cursor-column 40)))
      (check-equal "the first frame paints every row and drops rows past the height"
                   '(1 2 3)
                   (frame-painter-tests--painted-rows controls))
      (check-true "rows past the height are never written"
                  (not (search "four" controls)))
      (check-true "painting disables autowrap and the cursor until it finishes"
                  (and (eql 0 (search (format nil "~c[?25l~c[?7l" escape escape) controls))
                       (search (format nil "~c[?7h~c[?25h" escape escape) controls)))
      (check-true "the cursor is clamped to the screen"
                  (search (format nil "~c[3;10H" escape) controls)))
    (check-equal "an unchanged frame rewrites no rows"
                 nil
                 (frame-painter-tests--painted-rows
                  (frame-painter-tests--paint painter '("one" "two" "three")
                                              :height 3 :width 10)))
    (let ((controls (frame-painter-tests--paint painter '("one" "TWO")
                                                :height 3 :width 10
                                                :cursor-visible-p nil)))
      (check-equal "only changed rows are rewritten, including rows that became blank"
                   '(2 3)
                   (frame-painter-tests--painted-rows controls))
      (check-true "a hidden cursor stays hidden"
                  (search (format nil "~c[?25l" escape) controls :start2 2)))
    (check-equal "a width change repaints every row"
                 '(1 2 3)
                 (frame-painter-tests--painted-rows
                  (frame-painter-tests--paint painter '("one" "TWO")
                                              :height 3 :width 11)))
    (check-equal "a failed write leaves the next frame complete"
                 '(:failed (1 2 3))
                 (list (handler-case
                           (frame-painter-paint painter '("one" "TWO") (lambda (controls)
                                                                         (declare (ignore controls))
                                                                         (error "write failed"))
                                                :height 3 :width 11)
                         (error () :failed))
                       (frame-painter-tests--painted-rows
                        (frame-painter-tests--paint painter '("one" "TWO")
                                                    :height 3 :width 11))))
    (check-equal "the last written frame is readable and padded to the height"
                 #("one" "TWO" "")
                 (frame-painter-frame painter))
    (frame-painter-invalidate painter)
    (check-equal "an invalidated painter knows no frame"
                 nil
                 (frame-painter-frame painter))
    (check-equal "invalidating repaints every row"
                 '(1 2 3)
                 (frame-painter-tests--painted-rows
                  (frame-painter-tests--paint painter '("one" "TWO")
                                              :height 3 :width 11))))
  (values))
