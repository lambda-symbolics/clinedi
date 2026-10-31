;;;; -- Transcript viewport --

(in-package #:clinedi)

(defstruct (viewport-chunk
            (:constructor %make-viewport-chunk (text display regions tag))
            (:copier nil))
  "One appended transcript chunk kept unwrapped for reflow."
  (text "" :type string :read-only t)
  (display "" :type string :read-only t)
  (regions nil :type list :read-only t)
  (tag nil)
  (weight 0 :type (integer 0)))

(defstruct (viewport-row
            (:constructor %make-viewport-row (display chunk offset length))
            (:copier nil))
  "One wrapped display row anchored to the plain characters of its chunk.

CHUNK is the chunk's append number, counting evicted chunks, and OFFSET and
LENGTH locate the row's plain characters inside it."
  (display "" :type string :read-only t)
  (chunk 0 :type (integer 0) :read-only t)
  (offset 0 :type (integer 0) :read-only t)
  (length 0 :type (integer 0) :read-only t))

(defclass transcript-viewport ()
  ((chunks
    :initform      (make-array 0 :adjustable t :fill-pointer 0)
    :reader        transcript-viewport--chunks
    :documentation "Resident unwrapped chunks in append order.")
   (rows
    :initform      (make-array 0 :adjustable t :fill-pointer 0)
    :accessor      transcript-viewport--rows
    :documentation "Wrapped rows of the resident chunks at the current width.")
   (chunk-base
    :initform      0
    :accessor      transcript-viewport--chunk-base
    :documentation "The append number of the oldest resident chunk.")
   (row-base
    :initform      0
    :accessor      transcript-viewport--row-base
    :documentation "How many rows eviction has removed from the front.")
   (weight
    :initform      0
    :accessor      transcript-viewport--weight
    :documentation "Characters of text, display and wrapped rows the resident chunks hold.")
   (character-limit
    :initarg       :character-limit
    :initform      nil
    :reader        transcript-viewport-character-limit
    :documentation "The most resident characters before the oldest chunks are evicted, or NIL.")
   (evict-function
    :initarg       :evict-function
    :initform      nil
    :reader        transcript-viewport-evict-function
    :documentation "A function called with each evicted chunk's tag, oldest first, or NIL.")
   (width
    :initarg       :width
    :reader        transcript-viewport-width
    :documentation "The column count the rows are wrapped to.")
   (top
    :initform      nil
    :accessor      transcript-viewport-top
    :documentation "The first visible row, or NIL to follow the newest output.")
   (height
    :initform      1
    :reader        transcript-viewport-height
    :documentation "The row count of the last layout.")
   (maximum-top
    :initform      0
    :reader        transcript-viewport-maximum-top
    :documentation "The largest first row the last layout allowed."))
  (:documentation
   "A scrollable transcript of appended plain and styled text.

Chunks are kept unwrapped and every wrapped row remembers the characters it
shows, so a width change reflows the transcript while keeping the same first
visible line, and a screen position maps back to a character and its click
region. With a character limit, the oldest chunks are evicted once the
resident text, displays and rows exceed it; the application keeps the full
history and is told each evicted chunk's tag."))

(defun make-transcript-viewport (&key (width 80) character-limit evict-function)
  "Return an empty transcript viewport wrapping to WIDTH columns.

CHARACTER-LIMIT bounds the characters resident chunks may hold; the newest
chunk always stays. EVICT-FUNCTION receives the tag of each evicted chunk."
  (check-type width (integer 1))
  (check-type character-limit (or null (integer 1)))
  (check-type evict-function (or null function))
  (make-instance 'transcript-viewport :width           width
                                      :character-limit character-limit
                                      :evict-function  evict-function))

(defun transcript-viewport-row-count (viewport)
  "Return the number of resident wrapped rows in VIEWPORT."
  (length (transcript-viewport--rows viewport)))

(defun transcript-viewport-row-display (viewport index)
  "Return the trusted styled display of VIEWPORT's wrapped row INDEX."
  (viewport-row-display (aref (transcript-viewport--rows viewport) index)))

(defun transcript-viewport-row-text (viewport index)
  "Return the plain characters VIEWPORT's wrapped row INDEX shows."
  (let* ((row   (aref (transcript-viewport--rows viewport) index))
         (start (viewport-row-offset row)))
    (subseq (viewport-chunk-text (viewport--row-chunk viewport row))
            start
            (+ start (viewport-row-length row)))))

(defun transcript-viewport-chunk-count (viewport)
  "Return the number of resident chunks in VIEWPORT."
  (length (transcript-viewport--chunks viewport)))

(defun transcript-viewport-chunk-text (viewport index)
  "Return the plain text of VIEWPORT's resident chunk INDEX, oldest first."
  (viewport-chunk-text (aref (transcript-viewport--chunks viewport) index)))

(defun transcript-viewport-evicted-count (viewport)
  "Return how many chunks VIEWPORT has evicted."
  (transcript-viewport--chunk-base viewport))

(defun transcript-viewport-resident-characters (viewport)
  "Return the characters VIEWPORT's resident chunks hold, rows included."
  (transcript-viewport--weight viewport))

(defun transcript-viewport-last-tag (viewport)
  "Return the tag of VIEWPORT's newest chunk, or NIL when it has none."
  (let ((chunks (transcript-viewport--chunks viewport)))
    (and (plusp (length chunks))
         (viewport-chunk-tag (aref chunks (1- (length chunks)))))))

(defun (setf transcript-viewport-last-tag) (tag viewport)
  "Replace the tag of VIEWPORT's newest chunk with TAG and return TAG.

An empty VIEWPORT has no chunk to tag and is left unchanged."
  (let ((chunks (transcript-viewport--chunks viewport)))
    (when (plusp (length chunks))
      (setf (viewport-chunk-tag (aref chunks (1- (length chunks)))) tag))
    tag))

(defun transcript-viewport-following-p (viewport)
  "Return true when VIEWPORT follows its newest output."
  (null (transcript-viewport-top viewport)))

(defun transcript-viewport-append (viewport text display &key regions tag)
  "Append plain TEXT, its styled DISPLAY, and click REGIONS to VIEWPORT.

DISPLAY must present exactly TEXT's visible characters. REGIONS is a list of
(START END ACTION) character ranges of TEXT, as cl-termdown's widget regions
are. TAG is any value the evict function later receives for this chunk. A
final newline ends the chunk rather than adding an empty row. Past the
character limit the oldest chunks are evicted."
  (when (plusp (length text))
    (let* ((chunks (transcript-viewport--chunks viewport))
           (index  (+ (transcript-viewport--chunk-base viewport) (length chunks)))
           (chunk  (%make-viewport-chunk text display regions tag))
           (rows   (viewport--wrap-chunk chunk index (transcript-viewport-width viewport))))
      (vector-push-extend chunk chunks)
      (dolist (row rows)
        (vector-push-extend row (transcript-viewport--rows viewport)))
      (setf (viewport-chunk-weight chunk) (viewport--chunk-weight chunk rows))
      (incf (transcript-viewport--weight viewport) (viewport-chunk-weight chunk))
      (viewport--evict viewport)))
  viewport)

(defun transcript-viewport-resize (viewport width)
  "Reflow VIEWPORT to WIDTH columns, keeping its first visible line in view.

Return true when the width changed."
  (check-type width (integer 1))
  (unless (= width (transcript-viewport-width viewport))
    (let* ((old     (transcript-viewport--rows viewport))
           (top     (transcript-viewport-top viewport))
           (anchor  (and top (< top (length old)) (aref old top)))
           (rows    (make-array 0 :adjustable t :fill-pointer 0))
           (weight  0)
           (new-top nil))
      ;; Drop the old rows before wrapping so a reflow never holds two row sets.
      (setf (transcript-viewport--rows viewport) rows)
      (viewport--clear-tail old 0)
      (loop for chunk across (transcript-viewport--chunks viewport)
            for index from (transcript-viewport--chunk-base viewport)
            do (let ((chunk-rows (viewport--wrap-chunk chunk index width)))
                 (dolist (row chunk-rows)
                   (when (and anchor
                              (= index (viewport-row-chunk anchor))
                              (<= (viewport-row-offset row) (viewport-row-offset anchor)))
                     (setf new-top (length rows)))
                   (vector-push-extend row rows))
                 (setf (viewport-chunk-weight chunk) (viewport--chunk-weight chunk chunk-rows))
                 (incf weight (viewport-chunk-weight chunk))))
      (setf (transcript-viewport--weight viewport) weight
            (slot-value viewport 'width) width
            (transcript-viewport-top viewport) (and top (or new-top top)))
      (viewport--evict viewport)
      t)))

(defun transcript-viewport-layout (viewport height &key (extra-rows 0))
  "Fit VIEWPORT to HEIGHT rows and return the index of its first visible row.

EXTRA-ROWS counts rows the caller shows after the wrapped rows, such as
unfinished output, so following the newest output keeps them in view."
  (check-type height (integer 0))
  (check-type extra-rows (integer 0))
  (let* ((total       (+ (transcript-viewport-row-count viewport) extra-rows))
         (maximum-top (max 0 (- total height)))
         (top         (min (or (transcript-viewport-top viewport) maximum-top) maximum-top)))
    (setf (slot-value viewport 'height) height
          (slot-value viewport 'maximum-top) maximum-top)
    (when (transcript-viewport-top viewport)
      (setf (transcript-viewport-top viewport) top))
    top))

(defun transcript-viewport-scroll (viewport delta)
  "Move VIEWPORT DELTA rows; positive values move towards the newest output.

Reaching the last layout's maximum resumes following the newest output."
  (let* ((maximum (transcript-viewport-maximum-top viewport))
         (top     (max 0 (min maximum (+ (or (transcript-viewport-top viewport) maximum)
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

(defun transcript-viewport-jump (viewport direction predicate &key (align :top))
  "Scroll VIEWPORT to the nearest row in DIRECTION accepted by PREDICATE.

DIRECTION is -1 to search towards older rows and 1 towards newer ones.
PREDICATE receives a row's chunk text and the row's offset within it, and
accepts the first row of each section, such as a message header. With ALIGN
:TOP the accepted row becomes the first visible row, searching from the
current first row. With ALIGN :END the section it opens is shown ending on
the bottom row, so a reader's eyes stay where they were; the search starts
from the section under the bottom row, and a section taller than the window
shows its first row on top instead, and the newest section follows the newest
output again. A position inside the final page resumes following. Return true
when a row was found."
  (check-type direction (member -1 1))
  (check-type align (member :top :end))
  (let* ((rows    (transcript-viewport--rows viewport))
         (maximum (transcript-viewport-maximum-top viewport))
         (current (or (transcript-viewport-top viewport) maximum)))
    (labels ((accepted-p (index)
               (let ((row (aref rows index)))
                 (funcall predicate
                          (viewport-chunk-text (viewport--row-chunk viewport row))
                          (viewport-row-offset row))))

             (above (index)
               (loop for candidate from (1- index) downto 0
                     when (accepted-p candidate)
                       return candidate))

             (below (index)
               (loop for candidate from (1+ index) below (length rows)
                     when (accepted-p candidate)
                       return candidate))

             (section-end (start)
               (let ((end (1- (or (below start) (length rows)))))
                 (loop while (and (> end start)
                                  (zerop (viewport-row-length (aref rows end))))
                       do (decf end))
                 end))

             (show (top)
               (let ((top (max 0 (min top maximum))))
                 (setf (transcript-viewport-top viewport) (unless (= top maximum) top)))))
      (let ((target nil))
        (if (eq align :top)
            (progn
              (setf target (if (minusp direction) (above current) (below current)))
              (when target
                (show target)))
            (let* ((bottom  (min (1- (length rows))
                                 (+ current (1- (transcript-viewport-height viewport)))))
                   (section (and (>= bottom 0)
                                 (if (accepted-p bottom) bottom (above bottom)))))
              (setf target
                    (cond
                      ((plusp direction)
                       (below (max bottom 0)))
                      ((null section)
                       nil)
                      ;; The newest section would only resume following, so
                      ;; a jump from the tail moves past it.
                      ((and (< section current) (below section))
                       section)
                      (t
                       (above section))))
              (when target
                (let ((end (section-end target)))
                  (cond
                    ((> (1+ (- end target)) (transcript-viewport-height viewport))
                     (show target))
                    ((null (below target))
                     (transcript-viewport-follow viewport))
                    (t
                     (show (1+ (- end (transcript-viewport-height viewport))))))))))
        (and target t)))))

(defun transcript-viewport-hit (viewport column row)
  "Return what VIEWPORT shows at one-based screen COLUMN and ROW of the last layout.

Return three values: the action of the click region covering that character,
or NIL; the character's chunk text; and its index in that text. Positions past
the wrapped rows, outside the layout, or beyond a row's last cell return NIL."
  (block nil
    (let* ((rows       (transcript-viewport--rows viewport))
           (maximum    (transcript-viewport-maximum-top viewport))
           (top        (min (or (transcript-viewport-top viewport) maximum) maximum))
           (screen-row (1- row))
           (absolute   (+ top screen-row)))
      (when (or (minusp screen-row)
                (>= screen-row (transcript-viewport-height viewport))
                (>= absolute (length rows)))
        (return nil))
      (let* ((anchor (aref rows absolute))
             (chunk  (viewport--row-chunk viewport anchor))
             (text   (viewport-chunk-text chunk))
             (start  (viewport-row-offset anchor))
             (end    (min (length text) (+ start (viewport-row-length anchor))))
             (index  (viewport--column-character-index text column :start start :end end)))
        (when index
          (values (loop for (region-start region-end action) in (viewport-chunk-regions chunk)
                        when (and (<= region-start index) (< index region-end))
                          return action)
                  text
                  index))))))

(defun transcript-viewport-checkpoint (viewport)
  "Return an opaque state that TRANSCRIPT-VIEWPORT-ROLLBACK can return VIEWPORT to.

Only appends and scrolling are undone; take a new checkpoint after a resize.
Chunks evicted since the checkpoint stay evicted."
  (list (+ (transcript-viewport--chunk-base viewport)
           (length (transcript-viewport--chunks viewport)))
        (+ (transcript-viewport--row-base viewport)
           (transcript-viewport-row-count viewport))
        (transcript-viewport-top viewport)
        (transcript-viewport--row-base viewport)))

(defun transcript-viewport-rollback (viewport checkpoint)
  "Discard VIEWPORT's appends and scrolling since CHECKPOINT.

Discarded chunks and rows release their storage."
  (destructuring-bind (chunk-end row-end top row-base) checkpoint
    (let* ((chunks       (transcript-viewport--chunks viewport))
           (rows         (transcript-viewport--rows viewport))
           (chunk-count  (max 0 (- chunk-end (transcript-viewport--chunk-base viewport))))
           (row-count    (max 0 (- row-end (transcript-viewport--row-base viewport))))
           (evicted-rows (- (transcript-viewport--row-base viewport) row-base)))
      (when (< chunk-count (length chunks))
        (loop for index from chunk-count below (length chunks)
              do (decf (transcript-viewport--weight viewport)
                       (viewport-chunk-weight (aref chunks index))))
        (viewport--clear-tail chunks chunk-count))
      (when (< row-count (length rows))
        (viewport--clear-tail rows row-count))
      (setf (transcript-viewport-top viewport)
            (and top (max 0 (- top evicted-rows))))))
  viewport)

(defun viewport--row-chunk (viewport row)
  "Return the resident chunk ROW of VIEWPORT shows."
  (aref (transcript-viewport--chunks viewport)
        (- (viewport-row-chunk row) (transcript-viewport--chunk-base viewport))))

(defun viewport--chunk-weight (chunk rows)
  "Return the characters CHUNK and its wrapped ROWS hold, counting each click region."
  (+ (length (viewport-chunk-text chunk))
     (length (viewport-chunk-display chunk))
     (* 16 (length (viewport-chunk-regions chunk)))
     (loop for row in rows
           sum (+ 8 (length (viewport-row-display row))))))

(defun viewport--clear-tail (vector start)
  "Shorten adjustable VECTOR to START elements, releasing the references past it."
  (fill vector nil :start start)
  (setf (fill-pointer vector) start)
  vector)

(defun viewport--evict (viewport)
  "Evict VIEWPORT's oldest chunks while it holds more than its character limit.

The newest chunk always stays. Eviction continues to three quarters of the
limit, so its cost spreads over many appends. The first visible row keeps its
place, and each evicted chunk's tag goes to the evict function."
  (let ((limit  (transcript-viewport-character-limit viewport))
        (chunks (transcript-viewport--chunks viewport))
        (rows   (transcript-viewport--rows viewport)))
    (when (and limit
               (> (transcript-viewport--weight viewport) limit)
               (> (length chunks) 1))
      (let* ((target      (floor (* 3 limit) 4))
             (base        (transcript-viewport--chunk-base viewport))
             (chunk-count (let ((count  0)
                                (weight (transcript-viewport--weight viewport)))
                            (loop while (and (< count (1- (length chunks))) (> weight target))
                                  do (decf weight (viewport-chunk-weight (aref chunks count)))
                                     (incf count))
                            count))
             (row-count   (or (position-if (lambda (row)
                                             (>= (viewport-row-chunk row) (+ base chunk-count)))
                                           rows)
                              (length rows)))
             (tags        (loop for index below chunk-count
                                collect (viewport-chunk-tag (aref chunks index)))))
        (loop for index below chunk-count
              do (decf (transcript-viewport--weight viewport)
                       (viewport-chunk-weight (aref chunks index))))
        (replace chunks chunks :start2 chunk-count)
        (viewport--clear-tail chunks (- (length chunks) chunk-count))
        (replace rows rows :start2 row-count)
        (viewport--clear-tail rows (- (length rows) row-count))
        (incf (transcript-viewport--chunk-base viewport) chunk-count)
        (incf (transcript-viewport--row-base viewport) row-count)
        (let ((top (transcript-viewport-top viewport)))
          (when top
            (setf (transcript-viewport-top viewport) (max 0 (- top row-count)))))
        (let ((function (transcript-viewport-evict-function viewport)))
          (when function
            (dolist (tag tags)
              (funcall function tag)))))))
  viewport)

(defun viewport--wrap-chunk (chunk index width)
  "Wrap CHUNK, number INDEX, to WIDTH columns as rows anchored to its text."
  (let* ((text   (viewport-chunk-text chunk))
         (pairs  (wrap-styled-text text (viewport-chunk-display chunk) width))
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

(defun viewport--column-character-index (text column &key start end)
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
