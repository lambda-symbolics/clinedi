;;;; -- Live-region tests --

(in-package #:clinedi/tests)

(defun live-region-tests--count (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count 1
        do (setf start (+ position (length needle)))))

(defun live-region-tests--signals-error-p (function)
  "True when calling FUNCTION signals an error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error ()
      t)))

(defun run-live-region-tests ()
  "Run scrollback-safe live-region lifecycle regression tests."
  (let ((stream       (make-string-output-stream))
        (flushes      0)
        (writes       0)
        (write-log    (make-array 0 :adjustable t :fill-pointer 0))
        (cursor-shows 0)
        (last-write   ""))
    (let* ((write-function
             (lambda (text)
               (incf writes)
               (vector-push-extend text write-log)
               (incf cursor-shows
                     (live-region-tests--count (ansi-cursor-show) text))
               (setf last-write text)
               (write-string text stream)))
           (region
             (make-live-region
              :columns        6
              :write-function write-function
              :flush-function (lambda () (incf flushes))))
           (text (format nil "> abcdef~%x~%")))
      (live-region-present region text :cursor 5)
      (check-equal "initial repaint uses one terminal write" 1 writes)
      (check-equal "repaint hides the cursor before changing rows"
                   (ansi-cursor-hide)
                   (subseq last-write 0 (length (ansi-cursor-hide))))
      (check-equal "repaint restores the cursor after changing rows"
                   (ansi-cursor-show)
                   (subseq last-write
                           (- (length last-write)
                              (length (ansi-cursor-show)))))
      (check-true "presented live region is visible"
                  (live-region-visible-p region))
      (check-equal "wrapped live region row count"
                   4
                   (live-region-row-count region))
      (check-equal "wrapped live cursor row"
                   0
                   (live-region-cursor-row region))
      (check-equal "wrapped live cursor column"
                   5
                   (live-region-cursor-column region))
      (let ((previous-writes writes))
        (live-region-present region text :cursor 6)
        (check-equal "replacement repaint uses one terminal write"
                     1
                     (- writes previous-writes)))
      (let ((previous-flushes flushes)
            (previous-writes  writes)
            (replacement      (format nil "> updated~%")))
        (live-region-append-and-present
         region
         (format nil "STREAMED~%")
         replacement
         :cursor           3
         :appended-display (ansi-colorize (format nil "STREAMED~%") :blue)
         :display          (ansi-colorize replacement :green))
        (check-equal "atomic append and repaint uses one terminal write"
                     1
                     (- writes previous-writes))
        (check-equal "atomic append and repaint uses one terminal flush"
                     1
                     (- flushes previous-flushes))
        (check-equal "atomic append emits scrollback once"
                     1
                     (live-region-tests--count "STREAMED" last-write))
        (check-true "atomic append paints the replacement in the same frame"
                    (search "updated" last-write))
        (check-equal "atomic append records replacement cursor row"
                     0
                     (live-region-cursor-row region))
        (check-equal "atomic append records replacement cursor column"
                     3
                     (live-region-cursor-column region)))
      (live-region-set-cursor-visible region nil)
      (check-true "cursor visibility can be disabled for repeated updates"
                  (not (live-region-cursor-visible-p region)))
      (live-region-present region text :cursor 5)
      (check-equal "hidden-cursor repaint keeps the cursor hidden"
                   (ansi-cursor-hide)
                   (subseq last-write
                           (- (length last-write)
                              (length (ansi-cursor-hide)))))
      (live-region-set-cursor-visible region t)
      (check-true "cursor visibility can be restored"
                  (live-region-cursor-visible-p region))
      (check-equal "restoring cursor visibility applies immediately"
                   (ansi-cursor-show)
                   last-write)
      (live-region-append region (format nil "FINAL~%~%"))
      (let ((output (get-output-stream-string stream)))
        (check-equal "scrollback output is appended once"
                     1
                     (live-region-tests--count "FINAL" output))
        (check-true "live repaint never clears below"
                    (not (search (format nil "~c[J" (code-char 27))
                                 output))))
      (let ((first-resize-write writes))
        (live-region-resize region 12 :repaint-p nil)
        (check-equal "deferred resize emits no intermediate frame"
                     first-resize-write
                     writes)
        (live-region-present region text :cursor 5)
        (let* ((erasure (aref write-log first-resize-write))
               (expected-prefix
                 (concatenate 'string
                              (ansi-cursor-hide)
                              (clinedi:ansi-cursor-down 2))))
          (check-equal "wider resize retracts only reflowed rows"
                       expected-prefix
                       (subseq erasure 0 (length expected-prefix)))))
      (check-equal "wider resize reflows retained presentation"
                   3
                   (live-region-row-count region))
      (live-region-resize region 3)
      (check-equal "resize updates live-region width"
                   3
                   (live-region-columns region))
      (check-equal "resize reflows retained presentation"
                   5
                   (live-region-row-count region))
      (live-region-resize region 3 :maximum-rows 2)
      (check-equal "resize records the live-region row cap"
                   2
                   (live-region-maximum-rows region))
      (check-true "viewport caps retained multiline content"
                  (<= (live-region-row-count region) 2))
      (live-region-present region text
                           :cursor  (length text)
                           :display (ansi-colorize text :green))
      (check-true "viewport follows the cursor through styled content"
                  (<= (live-region-row-count region) 2))
      (live-region-suspend region)
      (check-true "suspended live region is hidden"
                  (not (live-region-visible-p region)))
      (check-equal "suspended live region has no painted rows"
                   0
                   (live-region-row-count region))
      (live-region-resume region)
      (check-true "resumed live region is visible"
                  (live-region-visible-p region))
      (let ((previous-shows cursor-shows))
        (call-with-live-region-suspended
         region
         (lambda ()
           (check-true "callback runs with region hidden"
                       (not (live-region-visible-p region)))))
        (check-equal "compound output reveals the cursor only once"
                     1
                     (- cursor-shows previous-shows)))
      (check-true "callback restores live region"
                  (live-region-visible-p region))
      (let ((styled (ansi-colorize text :green)))
        (live-region-present region text :cursor 0 :display styled))
      (let ((styled
              (cl-colorist:paint
               text
               :foreground (cl-colorist:indexed-color 114 :fallback :green)
               :background (cl-colorist:indexed-color 236 :fallback :black)
               :level      :indexed)))
        (live-region-present region text :cursor 0 :display styled)
        (check-true "live region accepts indexed foreground and background"
                    (search (format nil "~c[38;5;114;48;5;236m"
                                    (code-char 27))
                            last-write)))
      (check-true "visible-content mismatch is rejected"
                  (live-region-tests--signals-error-p
                   (lambda ()
                     (live-region-present region "plain"
                                          :display "different"))))
      (check-true "unmodeled geometry controls are rejected"
                  (live-region-tests--signals-error-p
                   (lambda ()
                     (live-region-present region
                                          (format nil "bad~c" #\tab)))))
      (live-region-dismiss region)
      (check-true "dismissed live region is hidden"
                  (not (live-region-visible-p region)))
      (check-true "dismissed live region restores terminal cursor visibility"
                  (live-region-cursor-visible-p region))
      (check-equal "dismissed live region has no rows"
                   0
                   (live-region-row-count region))
      (check-true "live-region operations flush terminal output"
                  (plusp flushes))))
  (values))
