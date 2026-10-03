;;;; -- Transcript viewport --

(in-package #:clinedi)

(defstruct (viewport-chunk
            (:constructor %make-viewport-chunk (text display regions))
            (:copier nil))
  "One appended transcript chunk kept unwrapped for reflow."
  (text "" :type string :read-only t)
  (display "" :type string :read-only t)
  (regions nil :type list :read-only t))

(defstruct (viewport-row
            (:constructor %make-viewport-row (display chunk offset length))
            (:copier nil))
  "One wrapped display row anchored to the plain characters of its chunk.

OFFSET and LENGTH locate the row's plain characters inside chunk CHUNK."
  (display "" :type string :read-only t)
  (chunk 0 :type (integer 0) :read-only t)
  (offset 0 :type (integer 0) :read-only t)
  (length 0 :type (integer 0) :read-only t))

(defclass transcript-viewport ()
  ((chunks
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :reader transcript-viewport--chunks
    :documentation "Unwrapped chunks in append order.")
   (rows
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor transcript-viewport--rows
    :documentation "Wrapped rows at the current width, extended only for new chunks.")
   (width
    :initarg :width
    :reader transcript-viewport-width
    :documentation "The column count the rows are wrapped to.")
   (top
    :initform nil
    :accessor transcript-viewport-top
    :documentation "The first visible row, or NIL to follow the newest output.")
   (height
    :initform 1
    :reader transcript-viewport-height
    :documentation "The row count of the last layout.")
   (maximum-top
    :initform 0
    :reader transcript-viewport-maximum-top
    :documentation "The largest first row the last layout allowed."))
  (:documentation
   "A scrollable transcript of appended plain and styled text.

Chunks are kept unwrapped and every wrapped row remembers the characters it
shows, so a width change reflows the transcript while keeping the same first
visible line, and a screen position maps back to a character and its click
region."))

(defun make-transcript-viewport (&key (width 80))
  "Return an empty transcript viewport wrapping to WIDTH columns."
  (check-type width (integer 1))
  (make-instance 'transcript-viewport :width width))

(defun transcript-viewport-row-count (viewport)
  "Return the number of wrapped rows in VIEWPORT."
  (length (transcript-viewport--rows viewport)))

(defun transcript-viewport-row-display (viewport index)
  "Return the trusted styled display of VIEWPORT's wrapped row INDEX."
  (viewport-row-display (aref (transcript-viewport--rows viewport) index)))

(defun transcript-viewport-following-p (viewport)
  "Return true when VIEWPORT follows its newest output."
  (null (transcript-viewport-top viewport)))

(defun transcript-viewport-append (viewport text display &key regions)
  "Append plain TEXT, its styled DISPLAY, and click REGIONS to VIEWPORT.

DISPLAY must present exactly TEXT's visible characters. REGIONS is a list of
(START END ACTION) character ranges of TEXT, as cl-termdown's widget regions
are. A final newline ends the chunk rather than adding an empty row."
  (when (plusp (length text))
    (let* ((chunks (transcript-viewport--chunks viewport))
           (index (length chunks))
           (chunk (%make-viewport-chunk text display regions)))
      (vector-push-extend chunk chunks)
      (dolist (row (viewport--wrap-chunk chunk index (transcript-viewport-width viewport)))
        (vector-push-extend row (transcript-viewport--rows viewport)))))
  viewport)

(defun transcript-viewport-resize (viewport width)
  "Reflow VIEWPORT to WIDTH columns, keeping its first visible line in view.

Return true when the width changed."
  (check-type width (integer 1))
  (unless (= width (transcript-viewport-width viewport))
    (let* ((old (transcript-viewport--rows viewport))
           (top (transcript-viewport-top viewport))
           (anchor (and top (< top (length old)) (aref old top)))
           (rows (make-array 0 :adjustable t :fill-pointer 0))
           (new-top nil))
      (loop for chunk across (transcript-viewport--chunks viewport)
            for index from 0
            do (dolist (row (viewport--wrap-chunk chunk index width))
                 (when (and anchor
                            (= index (viewport-row-chunk anchor))
                            (<= (viewport-row-offset row) (viewport-row-offset anchor)))
                   (setf new-top (length rows)))
                 (vector-push-extend row rows)))
      (setf (transcript-viewport--rows viewport) rows
            (slot-value viewport 'width) width
            (transcript-viewport-top viewport) (and top (or new-top top)))
      t)))

(defun transcript-viewport-layout (viewport height &key (extra-rows 0))
  "Fit VIEWPORT to HEIGHT rows and return the index of its first visible row.

EXTRA-ROWS counts rows the caller shows after the wrapped rows, such as
unfinished output, so following the newest output keeps them in view."
  (check-type height (integer 0))
  (check-type extra-rows (integer 0))
  (let* ((total (+ (transcript-viewport-row-count viewport) extra-rows))
         (maximum-top (max 0 (- total height)))
         (top (min (or (transcript-viewport-top viewport) maximum-top) maximum-top)))
    (setf (slot-value viewport 'height) height
          (slot-value viewport 'maximum-top) maximum-top)
    (when (transcript-viewport-top viewport)
      (setf (transcript-viewport-top viewport) top))
    top))

(defun transcript-viewport-scroll (viewport delta)
  "Move VIEWPORT DELTA rows; positive values move towards the newest output.

Reaching the last layout's maximum resumes following the newest output."
  (let* ((maximum (transcript-viewport-maximum-top viewport))
         (top (max 0 (min maximum (+ (or (transcript-viewport-top viewport) maximum)
                                     delta)))))
    (setf (transcript-viewport-top viewport) (unless (= top maximum) top))
    viewport))

(defun transcript-viewport-scroll-to-top (viewport)
  "Show VIEWPORT's oldest row first."
  (setf (transcript-viewport-top viewport) 0)
  viewport)

(defun transcript-viewport-follow (viewport)
  "Make VIEWPORT follow its newest output again."
  (setf (transcript-viewport-top viewport) nil)
  viewport)

(defun transcript-viewport-page-rows (viewport)
  "Return how many rows one page of VIEWPORT moves, keeping one row of context."
  (max 1 (1- (transcript-viewport-height viewport))))

(defun transcript-viewport-jump (viewport direction predicate)
  "Scroll VIEWPORT to the nearest row in DIRECTION accepted by PREDICATE.

DIRECTION is -1 to search above the first visible row and 1 below it.
PREDICATE receives a row's chunk text and the row's offset within it. A
target inside the final page resumes following. Return true when a row was
found."
  (check-type direction (member -1 1))
  (let* ((rows (transcript-viewport--rows viewport))
         (chunks (transcript-viewport--chunks viewport))
         (maximum (transcript-viewport-maximum-top viewport))
         (current (or (transcript-viewport-top viewport) maximum))
         (target
           (flet ((accepted-p (index)
                    (let ((row (aref rows index)))
                      (funcall predicate
                               (viewport-chunk-text (aref chunks (viewport-row-chunk row)))
                               (viewport-row-offset row)))))
             (if (minusp direction)
                 (loop for index from (1- current) downto 0
                       when (accepted-p index)
                         return index)
                 (loop for index from (1+ current) below (length rows)
                       when (accepted-p index)
                         return index)))))
    (when target
      (let ((top (min target maximum)))
        (setf (transcript-viewport-top viewport) (unless (= top maximum) top)))
      t)))

(defun transcript-viewport-hit (viewport column row)
  "Return what VIEWPORT shows at one-based screen COLUMN and ROW of the last layout.

Return three values: the action of the click region covering that character,
or NIL; the character's chunk text; and its index in that text. Positions past
the wrapped rows, outside the layout, or beyond a row's last cell return NIL."
  (block nil
    (let* ((rows (transcript-viewport--rows viewport))
           (maximum (transcript-viewport-maximum-top viewport))
           (top (min (or (transcript-viewport-top viewport) maximum) maximum))
           (screen-row (1- row))
           (absolute (+ top screen-row)))
      (when (or (minusp screen-row)
                (>= screen-row (transcript-viewport-height viewport))
                (>= absolute (length rows)))
        (return nil))
      (let* ((anchor (aref rows absolute))
             (chunk (aref (transcript-viewport--chunks viewport) (viewport-row-chunk anchor)))
             (text (viewport-chunk-text chunk))
             (start (viewport-row-offset anchor))
             (end (min (length text) (+ start (viewport-row-length anchor))))
             (index (viewport--column-character-index text start end column)))
        (when index
          (values (loop for (region-start region-end action) in (viewport-chunk-regions chunk)
                        when (and (<= region-start index) (< index region-end))
                          return action)
                  text
                  index))))))

(defun transcript-viewport-checkpoint (viewport)
  "Return an opaque state that TRANSCRIPT-VIEWPORT-ROLLBACK can return VIEWPORT to.

Only appends and scrolling are undone; take a new checkpoint after a resize."
  (list (length (transcript-viewport--chunks viewport))
        (transcript-viewport-row-count viewport)
        (transcript-viewport-top viewport)))

(defun transcript-viewport-rollback (viewport checkpoint)
  "Discard VIEWPORT's appends and scrolling since CHECKPOINT."
  (destructuring-bind (chunk-count row-count top) checkpoint
    (setf (fill-pointer (transcript-viewport--chunks viewport)) chunk-count
          (fill-pointer (transcript-viewport--rows viewport)) row-count
          (transcript-viewport-top viewport) top))
  viewport)

(defun viewport--wrap-chunk (chunk index width)
  "Wrap CHUNK, number INDEX, to WIDTH columns as rows anchored to its text."
  (let* ((text (viewport-chunk-text chunk))
         (pairs (wrap-styled-text text (viewport-chunk-display chunk) width))
         (offset 0))
    ;; A final newline begins the next append, rather than an extra empty row.
    (when (and (plusp (length text)) (char= (char text (1- (length text))) #\Newline))
      (setf pairs (butlast pairs)))
    (loop for (plain styled) in pairs
          for start = (or (search plain text :start2 offset) offset)
          collect (%make-viewport-row styled index start (length plain))
          do (setf offset (+ start (length plain)))
             (when (and (< offset (length text)) (char= (char text offset) #\Newline))
               (incf offset)))))

(defun viewport--column-character-index (text start end column)
  "Return the index of the character of TEXT between START and END shown at COLUMN.

COLUMN is one-based. Zero-width characters belong to the cell before them and
are never returned; a column past the row's last cell returns NIL."
  (loop with cells = 0
        for index from start below end
        for width = (text-cell-width (string (char text index)))
        do (when (and (plusp width)
                      (<= cells (1- column) (+ cells width -1)))
             (return index))
           (incf cells width)
        finally (return nil)))
