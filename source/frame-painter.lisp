;;;; -- Fullscreen frame painting --

(in-package #:clinedi)

(defclass frame-painter ()
  ((frame
    :initform nil
    :accessor frame-painter--frame
    :documentation "The rows of the last successfully written frame, or NIL to repaint fully.")
   (width
    :initform 0
    :accessor frame-painter--width
    :documentation "The column count of the last successfully written frame."))
  (:documentation
   "Paint fullscreen frames by rewriting only the rows that changed.

Rows are drawn at absolute positions with autowrap disabled, so a row that
overruns the width is clipped instead of scrolling the screen."))

(defun make-frame-painter ()
  "Return a painter whose first frame is painted completely."
  (make-instance 'frame-painter))

(defun frame-painter-frame (painter)
  "Return a fresh vector of the rows PAINTER last wrote, or NIL when none is known."
  (let ((frame (frame-painter--frame painter)))
    (and frame (copy-seq frame))))

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
or had another size. WRITE receives the complete control string and must
write and flush it. The frame is recorded only when WRITE returns; when it
exits non-locally PAINTER repaints completely next time. The cursor is left at
zero-based CURSOR-ROW and CURSOR-COLUMN, clamped to the screen, and shown when
CURSOR-VISIBLE-P."
  (check-type height (integer 1))
  (check-type width (integer 1))
  (let* ((frame (make-array height :initial-element ""))
         (previous (frame-painter--frame painter))
         (complete-p (or (null previous)
                         (/= height (length previous))
                         (/= width (frame-painter--width painter))))
         (recorded-p nil))
    (loop for row in rows
          for index from 0 below height
          do (setf (aref frame index) row))
    (unwind-protect
         (progn
           (funcall write
                    (frame-painter--controls frame
                                             (and (not complete-p) previous)
                                             :height height
                                             :width width
                                             :cursor-row cursor-row
                                             :cursor-column cursor-column
                                             :cursor-visible-p cursor-visible-p))
           (setf (frame-painter--frame painter) frame
                 (frame-painter--width painter) width
                 recorded-p t))
      (unless recorded-p
        (frame-painter-invalidate painter))))
  painter)

(defun frame-painter--controls (frame previous
                                &key height width cursor-row cursor-column cursor-visible-p)
  "Return the controls drawing FRAME's rows that differ from PREVIOUS, or every row."
  (let ((close-link (format nil "~c]8;;~c\\" +escape-character+ +escape-character+)))
    (with-output-to-string (output)
      (format output "~c[?25l~c[?7l" +escape-character+ +escape-character+)
      (loop for row across frame
            for index from 0
            when (or (null previous) (string/= row (aref previous index)))
              do (format output "~c[~d;1H~c[0m~a~c[2K~a~a~c[0m"
                         +escape-character+ (1+ index) +escape-character+ close-link
                         +escape-character+ row close-link +escape-character+))
      (format output "~c[~d;~dH~c[?7h~c[?25~:[l~;h~]"
              +escape-character+
              (1+ (max 0 (min cursor-row (1- height))))
              (1+ (max 0 (min cursor-column (1- width))))
              +escape-character+ +escape-character+
              cursor-visible-p))))
