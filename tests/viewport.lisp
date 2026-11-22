;;;; -- Transcript viewport tests --

(in-package #:clinedi/tests)

(defun viewport-tests--rows (viewport)
  "Return every wrapped display row of VIEWPORT."
  (loop for index below (transcript-viewport-row-count viewport)
        collect (transcript-viewport-row-display viewport index)))

(defun viewport-tests--append (viewport text &rest regions)
  "Append unstyled TEXT with REGIONS to VIEWPORT."
  (transcript-viewport-append viewport text text :regions regions))

(defun viewport-tests--selection ()
  "Test selecting a viewport's characters by dragging across its rows."
  (let ((viewport (make-transcript-viewport :width 6))
        (escape   (string (code-char 27))))
    (check-equal "an empty viewport has nothing to select"
                 '(nil nil)
                 (list (transcript-viewport-selection-start viewport 1 1)
                       (transcript-viewport-selection-text viewport)))
    (viewport-tests--append viewport (format nil "abcdefghij~%"))
    (viewport-tests--append viewport (format nil "# one~%body~%"))
    (transcript-viewport-layout viewport 4)
    (transcript-viewport-selection-start viewport 2 1)
    (transcript-viewport-selection-extend viewport 3 2)
    (check-equal "a drag selects from the press to the character under the pointer"
                 "bcdefghi"
                 (transcript-viewport-selection-text viewport))
    (check-equal "selected characters show in reverse video"
                 (list (format nil "a~A[7mbcdef~A[27m" escape escape)
                       (format nil "~A[7mghi~A[27mj" escape escape)
                       "# one")
                 (list (transcript-viewport-row-display viewport 0)
                       (transcript-viewport-row-display viewport 1)
                       (transcript-viewport-row-display viewport 2)))
    (transcript-viewport-selection-extend viewport 10 9)
    (check-equal "a selection past the last row reaches its end, across chunks"
                 (format nil "bcdefghij~%# one~%body~%")
                 (transcript-viewport-selection-text viewport))
    (transcript-viewport-selection-start viewport 2 2)
    (transcript-viewport-selection-extend viewport 1 1)
    (check-equal "a drag backwards selects the same way"
                 "abcdefgh"
                 (transcript-viewport-selection-text viewport))
    (transcript-viewport-selection-clear viewport)
    (check-equal "clearing ends the selection and its highlight"
                 '(nil "abcdef")
                 (list (transcript-viewport-selection-text viewport)
                       (transcript-viewport-row-display viewport 0)))))

(defun viewport-tests--selection-autoscroll ()
  "Step the selection and viewport together while a drag holds an edge."
  (let ((viewport (make-transcript-viewport :width 6)))
    (viewport-tests--append viewport (format nil "aa~%bb~%cc~%dd~%ee~%"))
    (transcript-viewport-layout viewport 3)
    (check-equal "an edge alone cannot start a drag"
                 nil (transcript-viewport-selection-autoscroll viewport 1 1))
    (transcript-viewport-selection-start viewport 2 2)
    (check-equal "only edges of the active selection scroll"
                 '(-1 -1 nil 1 nil)
                 (loop for row from 0 to 4 collect (transcript-viewport-selection-edge viewport row)))
    (check-equal "autoscroll extends the selection through the newly visible older row"
                 (list t 1 (format nil "bb~%cc~%dd"))
                 (list (transcript-viewport-selection-autoscroll viewport 1 1)
                       (transcript-viewport-top viewport)
                       (transcript-viewport-selection-text viewport)))
    (check-equal "scrolling down at the edge selects the newer row and follows the tail"
                 (list t t (format nil "d~%e"))
                 (list (transcript-viewport-selection-autoscroll viewport 1 3)
                       (transcript-viewport-following-p viewport)
                       (transcript-viewport-selection-text viewport)))
    (check-equal "the bottom boundary reports no motion"
                 nil (transcript-viewport-selection-autoscroll viewport 1 3))
    (transcript-viewport-layout viewport 3 :extra-rows 4)
    (check-equal "a layout showing only extra rows cannot start a selection"
                 nil (transcript-viewport-selection-start viewport 1 1))))

(defun viewport-tests--source-layout ()
  "Exercise source callbacks, local replacement, rollback and bounded ownership."
  (let* ((calls    0)
         (source   "abcdefghij")
         (layout   (lambda (width)
                     (incf calls)
                     (let ((text (if (< width 10)
                                     (format nil "abc~%defghij")
                                     source)))
                       (values text text (list (list 0 3 :source))))))
         (viewport (make-transcript-viewport :width 20)))
    (transcript-viewport-append viewport source source :tag :source
                                :layout-function layout :source-characters 10)
    (viewport-tests--append viewport "tail")
    (transcript-viewport-resize viewport 20)
    (check-equal "unchanged widths do no layout work" 0 calls)
    (let ((checkpoint (transcript-viewport-checkpoint viewport)))
      (transcript-viewport-resize viewport 8)
      (check-equal "narrow layout uses retained source" '("abc" "defghij" "tail")
                   (viewport-tests--rows viewport))
      (transcript-viewport-layout viewport 2)
      (transcript-viewport-scroll-to-top viewport)
      (check-equal "rerendered regions remain actionable" :source
                   (transcript-viewport-hit viewport 1 1))
      (transcript-viewport-selection-start viewport 1 2)
      (transcript-viewport-selection-extend viewport 3 2)
      (check-equal "selection starts after the inserted newline" "def"
                   (transcript-viewport-selection-text viewport))
      (transcript-viewport-layout viewport 1)
      (transcript-viewport-scroll viewport 1)
      (transcript-viewport-resize viewport 20)
      (check-equal "widening restores source layout" '("abcdefghij" "tail")
                   (mapcar #'ansi-strip (viewport-tests--rows viewport)))
      (check-equal "each width change invokes each source once" 2 calls)
      (check-equal "changed text clears selection and anchors the same chunk"
                   '(nil 0 nil)
                   (list (transcript-viewport-selection-text viewport)
                         (transcript-viewport-top viewport)
                         (transcript-viewport-following-p viewport)))
      (transcript-viewport-selection-clear viewport)
      (transcript-viewport-rollback viewport checkpoint)
      (transcript-viewport-resize viewport 8)
      (check-equal "rollback retains source ownership" 3 calls))
    (let ((tail (aref (clinedi::transcript-viewport--rows viewport) 2)))
      (transcript-viewport-replace viewport :source :text "new" :display "new")
      (check-equal "replacement neither rerenders nor rebuilds unrelated rows"
                   (list 3 tail)
                   (list calls (aref (clinedi::transcript-viewport--rows viewport) 1))))
    (let ((bounded (make-transcript-viewport :width 20 :character-limit 100)))
      (transcript-viewport-append bounded "a" "a" :tag :source
                                  :layout-function layout :source-characters 100)
      (viewport-tests--append bounded "b")
      (check-equal "logical source contributes to eviction accounting" 1
                   (transcript-viewport-evicted-count bounded))
      (transcript-viewport-resize bounded 9)
      (check-equal "evicted source callbacks are released" 3 calls))))

(defun viewport-tests--layout-failure ()
  "A failing source callback leaves the current viewport usable."
  (let ((viewport (make-transcript-viewport :width 20)))
    (viewport-tests--append viewport "static before callback")
    (transcript-viewport-append
     viewport "original" "original" :tag :source
     :layout-function                    (lambda (width)
                                           (declare (ignore width))
                                           (error "Layout failed")))
    (let ((before (viewport-tests--rows viewport))
          (weight (transcript-viewport-resident-characters viewport)))
      (handler-case (transcript-viewport-resize viewport 10)
        (error () nil))
      (check-equal "failed layout keeps rows, geometry and accounting"
                   (list before 20 weight)
                   (list (viewport-tests--rows viewport)
                         (transcript-viewport-width viewport)
                         (transcript-viewport-resident-characters viewport))))))

(defun viewport-tests--invalid-layout-regions ()
  "Reject invalid callback regions before publishing any new layout state."
  (dolist (regions '((("bad" 2 :action)) ((0 20 :action)) ((2 1 :action))
                     ((-1 2 :action)) ((0 2)) ((0 2 :action extra)) (bad)))
    (let ((viewport (make-transcript-viewport :width 20)))
      (transcript-viewport-append
       viewport "original" "original" :tag :source
       :layout-function                    (lambda (width)
                                             (declare (ignore width))
                                             (values "changed" "changed" regions)))
      (transcript-viewport-layout viewport 1)
      (transcript-viewport-scroll-to-top viewport)
      (transcript-viewport-selection-start viewport 1 1)
      (transcript-viewport-selection-extend viewport 3 1)
      (let ((before (viewport-tests--rows viewport))
            (weight (transcript-viewport-resident-characters viewport)))
        (check-equal "invalid callback regions signal before publication" t
                     (handler-case (progn (transcript-viewport-resize viewport 10) nil)
                       (type-error () t)))
        (check-equal "invalid regions retain rows, width, weight, selection and scroll"
                     (list before 20 weight "ori" 0)
                     (list (viewport-tests--rows viewport)
                           (transcript-viewport-width viewport)
                           (transcript-viewport-resident-characters viewport)
                           (transcript-viewport-selection-text viewport)
                           (transcript-viewport-top viewport)))))))

(defun viewport-tests--source-selection-policy ()
  "Preserve unchanged source offsets; clear selections when a newline is inserted."
  (let* ((source   "abcdefghij")
         (viewport (make-transcript-viewport :width 20)))
    (transcript-viewport-append
     viewport source source
     :layout-function (lambda (width)
                        (let ((text (if (< width 10) (format nil "abc~%defghij") source)))
                          (values text text nil))))
    (transcript-viewport-layout viewport 3)
    (transcript-viewport-selection-start viewport 4 1)
    (transcript-viewport-selection-extend viewport 6 1)
    (transcript-viewport-resize viewport 30)
    (check-equal "unchanged callback text preserves exact selection and follow mode"
                 '("def" t)
                 (list (transcript-viewport-selection-text viewport)
                       (transcript-viewport-following-p viewport)))
    (transcript-viewport-resize viewport 8)
    (check-equal "inserting a newline clears obsolete offsets after the common prefix"
                 '(nil t)
                 (list (transcript-viewport-selection-text viewport)
                       (transcript-viewport-following-p viewport)))))

(defun viewport-tests--replacement-selection-policy ()
  "Invalidate changed replacement offsets while preserving unchanged selections."
  (let ((source (format nil "abc~%defghij")))
    (dolist (replacement (list source "abcdefghij" "x" (format nil "z~%abc~%defghij")))
      (let ((viewport (make-transcript-viewport :width 8)))
        (transcript-viewport-append viewport source source :tag :source)
        (viewport-tests--append viewport "tail")
        (transcript-viewport-layout viewport 3)
        (transcript-viewport-selection-start viewport 1 2)
        (transcript-viewport-selection-extend viewport 3 2)
        (check-equal "selection starts after the replacement's common prefix" "def"
                     (transcript-viewport-selection-text viewport))
        (transcript-viewport-layout viewport 1)
        (transcript-viewport-scroll-to-top viewport)
        (transcript-viewport-scroll viewport 1)
        (transcript-viewport-replace viewport :source :text replacement :display replacement)
        (check-equal "replacement keeps follow mode and uses valid selection and scroll anchors"
                     (if (string= source replacement) '("def" 1 nil) '(nil 0 nil))
                     (list (transcript-viewport-selection-text viewport)
                           (transcript-viewport-top viewport)
                           (transcript-viewport-following-p viewport)))))))

(defun viewport-tests--prepend ()
  "Check the visible row, selection, hit regions and source layout after prepending."
  (let* ((viewport (make-transcript-viewport :width 4))
         (page     (make-transcript-viewport :width 8))
         (widths   nil)
         (layout   (lambda (width)
                     (push width widths)
                     (let ((text (if (= width 4) "HEADBODY" "original")))
                       (values text text '((4 8 :older)))))))
    (transcript-viewport-append page "original" "original" :tag :page :layout-function layout)
    (transcript-viewport-append viewport "abcdefgh" "abcdefgh" :tag :before
                                :regions '((4 8 :before)))
    (viewport-tests--append viewport "tail")
    (transcript-viewport-layout viewport 2)
    (transcript-viewport-selection-start viewport 2 1)
    (check-equal "prepending reflows the page, retains viewport content and returns copied tags"
                 (list viewport '(:page) nil)
                 (multiple-value-list (transcript-viewport-prepend viewport page)))
    (check-equal "the prior visible row and its character anchors are preserved"
                 '(3 "efgh" "f" (:before "abcdefgh" 5))
                 (list (transcript-viewport-top viewport)
                       (transcript-viewport-row-text viewport 3)
                       (transcript-viewport-selection-text viewport)
                       (multiple-value-list (transcript-viewport-hit viewport 2 1))))
    (check-equal "the page and its reflow source are independent of the destination"
                 '((4) "original" ("HEAD" "BODY" "abcd" "efgh" "tail"))
                 (list widths (transcript-viewport-chunk-text page 0)
                       (loop for i below (transcript-viewport-row-count viewport)
                             collect (transcript-viewport-row-text viewport i))))
    (setf (transcript-viewport-top viewport) 1)
    (check-equal "prefix hit regions use the layout at the destination width"
                 '(:older "HEADBODY" 4)
                 (multiple-value-list (transcript-viewport-hit viewport 1 1)))
    (transcript-viewport-resize viewport 8)
    (check-equal "a later resize relays out the copied source and reanchors the view"
                 '((8 4) 0 "original" :older)
                 (list widths (transcript-viewport-top viewport)
                       (transcript-viewport-row-text viewport 0)
                       (transcript-viewport-hit viewport 5 1))))
  (let* ((called   nil)
         (viewport (make-transcript-viewport :width 5 :character-limit 48
                    :evict-function (lambda (tag) (push tag called))))
         (page     (make-transcript-viewport :width 5)))
    (transcript-viewport-append viewport "AA" "AA" :tag :a)
    (transcript-viewport-append viewport "BBBB" "BBBB" :tag :b)
    (transcript-viewport-append viewport "CC" "CC" :tag :c)
    (transcript-viewport-append page "PPPP" "PPPP" :tag :page)
    (transcript-viewport-layout viewport 3)
    (transcript-viewport-selection-start viewport 1 1)
    (transcript-viewport-selection-extend viewport 1 3)
    (let ((checkpoint (transcript-viewport-checkpoint viewport)))
      (check-equal "prepending drops a contiguous newer suffix and reports its tags"
                   (list viewport '(:page) '(:b :c))
                   (multiple-value-list (transcript-viewport-prepend viewport page)))
      (check-equal "prepend eviction truncates the selection without a front eviction callback"
                   '(2 34 nil "AA" ("PPPP" "AA"))
                   (list (transcript-viewport-evicted-count viewport)
                         (transcript-viewport-resident-characters viewport)
                         called (transcript-viewport-selection-text viewport)
                         (loop for i below (transcript-viewport-row-count viewport)
                               collect (transcript-viewport-row-text viewport i))))
      (transcript-viewport-append viewport "DD" "DD" :tag :d)
      (check-equal "reusing the evicted suffix's indexes does not grow the selection"
                   "AA" (transcript-viewport-selection-text viewport))
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback restores the original selection and eviction count"
                   (list (format nil "AA~%BBBB~%C") 0 48)
                   (list (transcript-viewport-selection-text viewport)
                         (transcript-viewport-evicted-count viewport)
                         (transcript-viewport-resident-characters viewport)))))
  (let ((viewport (make-transcript-viewport :width 6 :character-limit 4))
        (page     (make-transcript-viewport :width 5)))
    (transcript-viewport-append page "oldest" "oldest" :tag :oldest)
    (transcript-viewport-append page "newer" "newer" :tag :newer)
    (transcript-viewport-append viewport "latest" "latest" :tag :latest)
    (check-equal "an oversized first chunk is kept and the remaining page is reported"
                 (list viewport '(:oldest) '(:newer :latest))
                 (multiple-value-list (transcript-viewport-prepend viewport page)))
    (setf (transcript-viewport-last-tag viewport) :retagged)
    (check-equal "prepended chunk metadata does not alias the source viewport"
                 '(:newer :retagged ("oldest"))
                 (list (transcript-viewport-last-tag page)
                       (transcript-viewport-last-tag viewport)
                       (viewport-tests--rows viewport))))
  (let ((viewport (make-transcript-viewport :width 6))
        (page     (make-transcript-viewport :width 5)))
    (viewport-tests--append viewport "before")
    (viewport-tests--append page "page")
    (transcript-viewport-append page "failure" "failure" :layout-function
                                (lambda (width)
                                  (declare (ignore width))
                                  (error "fixture failure")))
    (check-equal "a failing prefix layout does not publish a partial page"
                 '(t ("before"))
                 (list (handler-case (progn (transcript-viewport-prepend viewport page) nil)
                         (error () t))
                       (viewport-tests--rows viewport)))))

(defun viewport-tests--tagged-jump ()
  "Test bottom-aligned jumps among selected chunks, not incidental rows."
  (let ((viewport (make-transcript-viewport :width 10)))
    (dolist (entry (list (list :activity "noise")
                         (list :message (format nil "# a~%~%"))
                         (list :activity "# fake")
                         (list :message (format nil "# b~%one~%two~%three~%~%"))
                         (list :message (string #\Newline))
                         (list :side-status "# side")
                         (list :activity "activity")))
      (destructuring-bind (tag text) entry
        (transcript-viewport-append viewport text text :tag tag)))
    (transcript-viewport-layout viewport 2)
    (flet ((accepted-p (tag text)
             (and (member tag '(:message :side-status))
                  (or (char= (char text 0) #\#) (char= (char text 0) #\Newline)))))
      (dolist (case '((-1 t 6) (-1 t 0) (-1 nil 0) (1 t 6) (1 t 9) (1 nil 9)))
        (destructuring-bind (direction moved-p top) case
          (check-equal "tagged jumps skip activity and blank chunks and bottom-align even tall sections"
                       (list moved-p top)
                       (list (transcript-viewport-jump-tagged viewport direction #'accepted-p)
                             (transcript-viewport-top viewport)))))
      (transcript-viewport-append viewport "# last" "# last" :tag :message)
      (transcript-viewport-layout viewport 2 :extra-rows 1)
      (check-equal "a target ending at the last resident row resumes following extra output"
                   '(t t)
                   (list (transcript-viewport-jump-tagged viewport 1 #'accepted-p)
                         (transcript-viewport-following-p viewport)))
      (transcript-viewport-scroll-to-top viewport)
      (transcript-viewport-layout viewport 1)
      (check-equal "a forward jump before the first accepted chunk reaches it"
                   '(t 1)
                   (list (transcript-viewport-jump-tagged viewport 1 #'accepted-p)
                         (transcript-viewport-top viewport))))))

(defun run-viewport-tests ()
  "Run transcript viewport regression tests."
  (viewport-tests--selection)
  (viewport-tests--source-layout)
  (viewport-tests--layout-failure)
  (viewport-tests--invalid-layout-regions)
  (viewport-tests--source-selection-policy)
  (viewport-tests--replacement-selection-policy)
  (viewport-tests--prepend)
  (viewport-tests--tagged-jump)
  (viewport-tests--selection-autoscroll)
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
  (let ((viewport (make-transcript-viewport :width 20)))
    (dolist (chunk (list (format nil "# a~%a1~%~%") (format nil "# b~%b1~%b2~%~%")
                         (format nil "# c~%c1~%~%") (format nil "# d~%d1~%~%")))
      (viewport-tests--append viewport chunk))
    (transcript-viewport-layout viewport 4)
    (flet ((header-p (text offset)
             (and (< offset (length text)) (char= (char text offset) #\#)))
           (window ()
             (let ((top (or (transcript-viewport-top viewport)
                            (transcript-viewport-maximum-top viewport))))
               (loop for index from top below (min (+ top 4)
                                                   (transcript-viewport-row-count viewport))
                     collect (transcript-viewport-row-text viewport index)))))
      (check-equal "an end-aligned jump up ends the previous section on the bottom row"
                   '(t ("b2" "" "# c" "c1"))
                   (list (transcript-viewport-jump viewport -1 #'header-p :align :end)
                         (window)))
      (check-equal "the next jump up starts from the section on the bottom row"
                   '(t ("" "# b" "b1" "b2"))
                   (list (transcript-viewport-jump viewport -1 #'header-p :align :end)
                         (window)))
      (check-equal "an end-aligned jump down ends the following section on the bottom row"
                   '(t ("b2" "" "# c" "c1"))
                   (list (transcript-viewport-jump viewport 1 #'header-p :align :end)
                         (window)))
      (check-equal "reaching the newest section follows again, past its blank separator"
                   '(t nil)
                   (list (transcript-viewport-jump viewport 1 #'header-p :align :end)
                         (transcript-viewport-top viewport)))
      (transcript-viewport-layout viewport 2)
      (transcript-viewport-jump viewport -1 #'header-p :align :end)
      (transcript-viewport-jump viewport -1 #'header-p :align :end)
      (check-equal "a section taller than the window shows its first row on top"
                   "# b"
                   (transcript-viewport-row-text viewport (transcript-viewport-top viewport)))))
  (let ((viewport (make-transcript-viewport :width 10))
        (text     (format nil "猫 copy x~%")))
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
  (let ((viewport (make-transcript-viewport :width 10)))
    (viewport-tests--append viewport "checkpoint width~%")
    (let ((checkpoint (transcript-viewport-checkpoint viewport)))
      (check-true "resize changes the checkpoint width"
                  (transcript-viewport-resize viewport 20))
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback restores the checkpoint width"
                   10
                   (transcript-viewport-width viewport))
      (check-true "resize after rollback reflows at the restored width"
                  (transcript-viewport-resize viewport 20))))
  (run-viewport-replacement-tests)
  (run-viewport-residency-tests)
  (values))

(defun run-viewport-residency-tests ()
  "Run bounded transcript residency regression tests."
  (let* ((evicted  nil)
         (viewport (make-transcript-viewport :width           10
                                             :character-limit 600
                                             :evict-function  (lambda (tag) (push tag evicted))))
         (peak     0))
    (dotimes (index 200)
      (transcript-viewport-append viewport
                                  (format nil "chunk ~3,'0D twenty~%" index)
                                  (format nil "chunk ~3,'0D twenty~%" index)
                                  :tag index)
      (setf peak (max peak (transcript-viewport-resident-characters viewport))))
    (check-equal "resident characters never exceed the limit"
                 t
                 (<= peak 600))
    (check-equal "evicted tags arrive oldest first and resident chunks complete the history"
                 (list t 200)
                 (list (equal (reverse evicted)
                              (loop for index below (length evicted) collect index))
                       (+ (transcript-viewport-evicted-count viewport)
                          (transcript-viewport-chunk-count viewport))))
    (check-equal "the oldest resident row belongs to the oldest resident chunk"
                 (format nil "chunk ~3,'0D" (length evicted))
                 (transcript-viewport-row-text viewport 0))
    (transcript-viewport-layout viewport 3)
    (transcript-viewport-scroll viewport -4)
    (let ((anchored (transcript-viewport-row-text viewport (transcript-viewport-top viewport))))
      (dotimes (index 3)
        (transcript-viewport-append viewport (format nil "late ~D~%" index)
                                    (format nil "late ~D~%" index) :tag (+ 200 index)))
      (check-equal "eviction keeps a scrolled view on the same row"
                   anchored
                   (transcript-viewport-row-text viewport (transcript-viewport-top viewport)))
      (transcript-viewport-resize viewport 30)
      (check-equal "a reflow after eviction keeps the anchor and the bound"
                   '(t t)
                   (list (not (null (search anchored
                                            (transcript-viewport-row-text
                                             viewport (transcript-viewport-top viewport)))))
                         (<= (transcript-viewport-resident-characters viewport) 600))))
    (let ((checkpoint (transcript-viewport-checkpoint viewport))
          (count      (transcript-viewport-chunk-count viewport))
          (rows       (transcript-viewport-row-count viewport))
          (weight     (transcript-viewport-resident-characters viewport)))
      (transcript-viewport-append viewport "rolled back" "rolled back" :tag :rolled-back)
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback releases the appended chunk and its rows"
                   (list count rows weight)
                   (list (transcript-viewport-chunk-count viewport)
                         (transcript-viewport-row-count viewport)
                         (transcript-viewport-resident-characters viewport)))
      (transcript-viewport-append viewport "after rollback" "after rollback" :tag :after-rollback)
      (check-equal "append after rollback keeps adjustable storage"
                   :after-rollback
                   (transcript-viewport-last-tag viewport))
      (check-equal "append after rollback remains resident"
                   "after rollback"
                   (transcript-viewport-chunk-text
                    viewport (1- (transcript-viewport-chunk-count viewport)))))
    (setf (transcript-viewport-last-tag viewport) :retagged)
    (check-equal "the newest chunk can be retagged"
                 :retagged
                 (transcript-viewport-last-tag viewport)))
  (let ((viewport (make-transcript-viewport :width 10)))
    (transcript-viewport-append viewport "before" "before" :tag :before)
    (let ((checkpoint (transcript-viewport-checkpoint viewport)))
      (setf (transcript-viewport-last-tag viewport) :changed)
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback restores a chunk tag changed after checkpoint"
                   :before
                   (transcript-viewport-last-tag viewport))
      (transcript-viewport-append viewport "after" "after" :tag :after)
      (check-equal "a restored checkpoint remains usable after append"
                   '(t "replaced")
                   (list (transcript-viewport-replace viewport :after
                                                      :text "replaced"
                                                      :display "replaced")
                         (transcript-viewport-chunk-text
                          viewport (1- (transcript-viewport-chunk-count viewport)))))))
  (let ((viewport (make-transcript-viewport :width 10 :character-limit 10)))
    (transcript-viewport-append viewport "a long single chunk" "a long single chunk")
    (check-equal "the newest chunk stays even past the limit"
                 1
                 (transcript-viewport-chunk-count viewport)))
  (values))

(defun run-viewport-replacement-tests ()
  "Run transcript chunk replacement regression tests."
  (let ((viewport (make-transcript-viewport :width 5)))
    (transcript-viewport-append viewport "first\n" "first\n" :tag (list :first))
    (transcript-viewport-append viewport "middle\n" "middle\n" :tag (list :middle))
    (transcript-viewport-append viewport "last\n" "last\n" :tag (list :last))
    (check-equal "replacement matches tags with EQUAL"
                 t
                   (transcript-viewport-replace viewport (list :middle)
                                                   :text  "a much longer middle\n"
                                                 :display "a much longer middle\n"))
    (check-equal "replacement preserves text and tag"
                 "a much longer middle\n"
                 (transcript-viewport-chunk-text viewport 1))
    (check-equal "missing tags return NIL"
                 nil
                 (transcript-viewport-replace viewport :missing :text "x" :display "x"))
    (let ((checkpoint (transcript-viewport-checkpoint viewport))
          (before     (transcript-viewport-chunk-text viewport 1))
          (weight     (transcript-viewport-resident-characters viewport)))
      (transcript-viewport-replace viewport (list :middle)
                                    :text    "replacement replacement"
                                    :display "replacement replacement")
      (transcript-viewport-rollback viewport checkpoint)
      (check-equal "rollback restores a replaced chunk and its weight"
                   (list before weight)
                   (list (transcript-viewport-chunk-text viewport 1)
                         (transcript-viewport-resident-characters viewport)))
      (transcript-viewport-append viewport "tail" "tail" :tag :tail)
      (check-equal "replacement after rollback can append and replace"
                   '(t "tail replaced")
                   (list (transcript-viewport-replace viewport :tail
                                                      :text "tail replaced"
                                                      :display "tail replaced")
                         (transcript-viewport-chunk-text viewport 3))))
   (values)))
