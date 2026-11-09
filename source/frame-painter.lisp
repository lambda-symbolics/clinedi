;;;; -- Fullscreen frame painting --

(in-package #:clinedi)

(defclass frame-painter ()
  ((frame
    :initform      nil
    :accessor      frame-painter--frame
    :documentation "The last successfully written snapshot, or NIL to repaint fully."))
  (:documentation
   "Paint fullscreen frames by rewriting only the rows that changed.

Rows are drawn at absolute positions with autowrap disabled, so a row that
overruns the width is clipped instead of scrolling the screen."))

(defun make-frame-painter ()
  "Return a painter whose first frame is painted completely."
  (make-instance 'frame-painter))

(defun frame-painter-frame (painter)
  "Return a fresh vector of fresh rows PAINTER last wrote, or NIL when unknown."
  (let ((frame (frame-painter--frame painter)))
    (and frame (frame-snapshot-rows frame))))

(defun frame-painter-invalidate (painter)
  "Make PAINTER's next frame repaint every row, as after the screen was disturbed."
  (setf (frame-painter--frame painter) nil)
  painter)

(defun frame-painter-paint (painter rows write
                            &key height width (cursor-row 0) (cursor-column 0)
                                 (cursor-visible-p t))
  "Paint trusted display ROWS on a HEIGHT by WIDTH screen through WRITE.

ROWS beyond HEIGHT are dropped and missing rows are blank. Only rows that
differ from PAINTER's last frame are rewritten, unless that frame is unknown
or had another size. WRITE receives one complete control string bracketed by
DEC 2026 synchronized-output controls and must write and flush it. The frame
is recorded only when WRITE returns; when it exits non-locally PAINTER
repaints completely next time. The cursor is left at zero-based CURSOR-ROW
and CURSOR-COLUMN, clamped to the screen, and shown when CURSOR-VISIBLE-P."
  (let ((recorded-p nil))
    (unwind-protect
         (let ((frame (make-frame-snapshot rows :height height :width width)))
           (multiple-value-bind (changed complete-p)
               (frame-snapshot-delta frame (frame-painter--frame painter))
             ;; Sparse complete frames omit blanks; a terminal repaint must clear them.
             (when complete-p
               (setf changed (loop for row across (frame-snapshot--rows frame)
                                   for index from 0
                                   collect (list index row))))
             (funcall write
                      (frame-painter--controls changed
                                               :height           height
                                               :width            width
                                               :cursor-row       cursor-row
                                               :cursor-column    cursor-column
                                               :cursor-visible-p cursor-visible-p))
             (setf (frame-painter--frame painter) frame
                   recorded-p t)))
      (unless recorded-p
        (frame-painter-invalidate painter))))
  painter)

(defun frame-painter--controls (indexed-rows
                                &key height width cursor-row cursor-column cursor-visible-p)
  "Return the controls drawing INDEXED-ROWS and positioning the cursor."
  (let ((close-link (format nil "~c]8;;~c\\" *escape-character* *escape-character*)))
    (with-output-to-string (output)
      (format output "~c[?2026h~c[?25l~c[?7l"
              *escape-character* *escape-character* *escape-character*)
      (loop for (index row) in indexed-rows
            do (format output "~c[~d;1H~c[0m~a~c[2K~a~a~c[0m"
                       *escape-character* (1+ index) *escape-character* close-link
                       *escape-character* row close-link *escape-character*))
      (format output "~c[~d;~dH~c[?7h~c[?25~:[l~;h~]~c[?2026l"
              *escape-character*
              (1+ (max 0 (min cursor-row (1- height))))
              (1+ (max 0 (min cursor-column (1- width))))
              *escape-character* *escape-character*
              cursor-visible-p *escape-character*))))
