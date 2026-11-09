;;;; -- Terminal frame snapshot tests --

(in-package #:clinedi/tests)

(defun run-frame-snapshot-tests ()
  "Check normalized geometry, sparse reconstruction, and snapshot ownership."
  (dolist (case '((nil 2 4 ("" ""))
                  (("one") 3 1 ("one" "" ""))
                  (("one" "two" "three") 2 10 ("one" "two"))
                  (#("wide text") 1 1 ("wide text"))))
    (destructuring-bind (rows height width expected) case
      (let ((snapshot (make-frame-snapshot rows :height height :width width)))
        (check-equal "snapshot normalizes rows without clipping" expected
                     (coerce (frame-snapshot-rows snapshot) 'list))
        (check-equal "snapshot records geometry" (list height width)
                     (list (frame-snapshot-height snapshot) (frame-snapshot-width snapshot))))))
  (dolist (geometry '((0 1) (1 0) (-1 2) (2 -1) (nil 2) (2 1/2)))
    (destructuring-bind (height width) geometry
      (dolist (operation (list (lambda () (make-frame-snapshot nil :height height :width width))
                              (lambda () (frame-snapshot-apply nil nil :height height :width width
                                                                       :complete-p t))))
        (check-true "invalid geometry signals a type error"
                    (handler-case (progn (funcall operation) nil)
                      (type-error () t))))))
  (dolist (case '((nil ("a" "" "c") 3 4 ((0 "a") (2 "c")) t)
                  ((("a" "" "c") 3 4) ("a" "" "c") 3 4 nil nil)
                  ((("a" "b" "c") 3 4) ("a" "" "C") 3 4 ((1 "") (2 "C")) nil)
                  ((("a" "b") 2 4) ("a" "") 2 5 ((0 "a")) t)
                  ((("a" "b") 2 4) ("a" "" "") 3 4 ((0 "a")) t)
                  ((("a" "b") 2 4) nil 2 4 ((0 "") (1 "")) nil)))
    (destructuring-bind (baseline rows height width expected complete) case
      (let* ((previous (when baseline
                         (destructuring-bind (old-rows old-height old-width) baseline
                           (make-frame-snapshot old-rows :height old-height :width old-width))))
             (snapshot (make-frame-snapshot rows :height height :width width)))
        (multiple-value-bind (delta complete-p) (frame-snapshot-delta snapshot previous)
          (check-equal "delta selects ordered rows" expected delta)
          (check-equal "delta reports completeness" complete complete-p)
          (let ((applied (frame-snapshot-apply previous delta :height height :width width
                                                            :complete-p complete-p)))
            (check-equal "applying a delta reconstructs its snapshot"
                         (coerce (frame-snapshot-rows snapshot) 'list)
                         (coerce (frame-snapshot-rows applied) 'list)))))))
  (let ((previous (make-frame-snapshot '("a" "b") :height 2 :width 4)))
    (dolist (case (list (list nil 2 4) (list previous 3 4) (list previous 2 5)))
      (destructuring-bind (baseline height width) case
        (check-equal "incremental apply requires matching baseline" nil
                     (frame-snapshot-apply baseline '((0 "x")) :height height :width width))))
    (dolist (complete-p '(nil t))
      (let ((applied (frame-snapshot-apply previous '((1 "first") (9 "ignored") (1 "last"))
                                                    :height 2 :width 4 :complete-p complete-p)))
        (check-equal "apply ignores high indices and keeps last duplicate"
                     (list (if complete-p "" "a") "last")
                     (coerce (frame-snapshot-rows applied) 'list))))
    (check-equal "apply does not modify baseline" '("a" "b")
                 (coerce (frame-snapshot-rows previous) 'list)))
  (let* ((text     (copy-seq "owned"))
         (input    (vector text))
         (snapshot (make-frame-snapshot input :height 2 :width 1)))
    (setf (char text 0) #\X
          (aref input 0) "replaced")
    (let ((rows (frame-snapshot-rows snapshot)))
      (setf (char (aref rows 0) 1) #\Y
            (aref rows 1) "replaced"))
    (dolist (previous (list nil (make-frame-snapshot nil :height 2 :width 1)))
      (let ((delta (frame-snapshot-delta snapshot previous)))
        (setf (char (second (first delta)) 2) #\Z
              (first (first delta)) 1)))
    (check-equal "snapshot isolates constructor, accessor, and delta mutation"
                 '("owned" "") (coerce (frame-snapshot-rows snapshot) 'list))
    (dolist (complete-p '(nil t))
      (let* ((replacement (copy-seq "fresh"))
             (entries     (list (list 1 replacement)))
             (applied     (frame-snapshot-apply snapshot entries :height 2 :width 1
                                                               :complete-p complete-p)))
        (setf (char replacement 0) #\X
              (second (first entries)) "replaced"
              (char (aref (frame-snapshot-rows applied) 1) 1) #\Y)
        (check-equal "apply owns replacement strings and exposes copies"
                     (list (if complete-p "" "owned") "fresh")
                     (coerce (frame-snapshot-rows applied) 'list)))))
  (values))
