;;;; -- Immutable terminal frame snapshots --

(in-package #:clinedi)

(defclass frame-snapshot ()
  ((height
    :initarg       :height
    :reader        frame-snapshot-height
    :documentation "The positive row count.")
   (width
    :initarg       :width
    :reader        frame-snapshot-width
    :documentation "The positive column count, without clipping row text.")
   (rows
    :initarg       :rows
    :reader        frame-snapshot--rows
    :documentation "An owned vector of owned strings, never exposed directly."))
  (:documentation "An opaque, logically immutable snapshot of trusted display rows."))

(defun make-frame-snapshot (rows &key height width)
  "Copy trusted string ROWS into a HEIGHT by WIDTH snapshot.

HEIGHT and WIDTH must be positive integers. ROWS is a sequence; excess rows
are dropped and missing rows are blank. WIDTH describes geometry only and
does not clip text. Neither input strings nor their container are retained."
  (check-type height (integer 1))
  (check-type width (integer 1))
  (let ((normalized (make-array height :initial-element "")))
    (loop for row across (coerce rows 'vector)
          for index from 0 below height
          do (setf (aref normalized index) (copy-seq row)))
    (make-instance 'frame-snapshot :height height :width width :rows normalized)))

(defun frame-snapshot-rows (snapshot)
  "Return a fresh vector of fresh strings from SNAPSHOT."
  (map 'vector #'copy-seq (frame-snapshot--rows snapshot)))

(defun frame-snapshot--same-size-p (snapshot previous)
  "Return whether PREVIOUS exists with SNAPSHOT's geometry."
  (and previous
       (= (frame-snapshot-height snapshot) (frame-snapshot-height previous))
       (= (frame-snapshot-width snapshot) (frame-snapshot-width previous))))

(defun frame-snapshot-delta (snapshot previous)
  "Return indexed rows and a complete-p flag as two values.

Each row is a fresh (zero-based-index text) list with fresh text, in increasing
index order. An absent or differently sized PREVIOUS requires a complete
frame, whose blank rows are omitted. Otherwise include only changed rows,
including rows changed to blank. Neither snapshot is modified."
  (let ((complete-p (not (frame-snapshot--same-size-p snapshot previous))))
    (values
     (loop for row across (frame-snapshot--rows snapshot)
           for index from 0
           when (if complete-p
                    (plusp (length row))
                    (string/= row (aref (frame-snapshot--rows previous) index)))
             collect (list index (copy-seq row)))
     complete-p)))

(defun frame-snapshot-apply (previous indexed-rows &key height width complete-p)
  "Return a snapshot applying trusted INDEXED-ROWS, or NIL without a baseline.

HEIGHT and WIDTH must be positive integers. A complete frame starts with blank
rows; an incremental frame requires PREVIOUS with matching dimensions or
returns NIL. Entries are (zero-based-index text) lists. Indices at or above
HEIGHT are ignored; duplicate indices use the last text. Copy all retained
strings. Callers own resource limits and wire validation."
  (check-type height (integer 1))
  (check-type width (integer 1))
  (when (or complete-p
            (and previous
                 (= height (frame-snapshot-height previous))
                 (= width (frame-snapshot-width previous))))
    (let ((rows (if complete-p
                    (make-array height :initial-element "")
                    (frame-snapshot-rows previous))))
      (dolist (entry indexed-rows)
        (destructuring-bind (index text) entry
          (when (< index height)
            (setf (aref rows index) (copy-seq text)))))
      (make-instance 'frame-snapshot :height height :width width :rows rows))))
