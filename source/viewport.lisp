;;;; -- Transcript viewport --

(in-package #:clinedi)

(defstruct (viewport-chunk
            (:constructor %make-viewport-chunk
                (text display regions tag &optional layout-function (source-characters 0)))
            (:copier nil))
  "One appended transcript chunk with optional source-backed layout."
  (text "" :type string :read-only t)
  (display "" :type string :read-only t)
  (regions nil :type list :read-only t)
  (tag nil)
  (layout-function nil :type (or null function) :read-only t)
  (source-characters 0 :type (integer 0) :read-only t)
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
    :accessor      transcript-viewport--chunks
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
    :documentation "The largest first row the last layout allowed.")
   (selection-anchor
    :initform      nil
    :accessor      transcript-viewport--selection-anchor
    :documentation "Where the selection began, as (CHUNK . INDEX), or NIL without one.")
   (selection-head
    :initform      nil
    :accessor      transcript-viewport--selection-head
    :documentation "Where the selection reaches now, as (CHUNK . INDEX), or NIL without one."))
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
  "Return the trusted styled display of VIEWPORT's wrapped row INDEX.

A row the selection covers shows its plain characters, the selected ones in
reverse video."
  (let ((row (aref (transcript-viewport--rows viewport) index)))
    (multiple-value-bind (from to) (viewport--row-selection viewport row)
      (if from
          (let ((text (transcript-viewport-row-text viewport index))
                (base (viewport-row-offset row)))
            (format nil "~A~c[7m~A~c[27m~A"
                    (subseq text 0 (- from base))
                    *escape-character*
                    (subseq text (- from base) (- to base))
                    *escape-character*
                    (subseq text (- to base))))
          (viewport-row-display row)))))

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

(defun transcript-viewport-append (viewport text display
                                   &key regions tag layout-function (source-characters 0))
  "Append TEXT, styled DISPLAY, and click REGIONS to VIEWPORT.

DISPLAY presents exactly TEXT's visible characters; REGIONS contains
(START END ACTION) ranges. TAG identifies the chunk for replacement/eviction.
LAYOUT-FUNCTION, when supplied, returns fresh TEXT, DISPLAY and REGIONS for a
new width. Initial TEXT and DISPLAY already use the viewport width. Count
characters retained by its source in SOURCE-CHARACTERS for bounded eviction.
A final newline ends the chunk rather than adding an empty row."
  (check-type layout-function (or null function))
  (check-type source-characters (integer 0))
  (when (plusp (length text))
    (let* ((chunks (transcript-viewport--chunks viewport))
           (index  (+ (transcript-viewport--chunk-base viewport) (length chunks)))
           (chunk  (%make-viewport-chunk text display regions tag layout-function source-characters))
           (rows   (viewport--wrap-chunk chunk index (transcript-viewport-width viewport))))
      (vector-push-extend chunk chunks)
      (dolist (row rows)
        (vector-push-extend row (transcript-viewport--rows viewport)))
      (setf (viewport-chunk-weight chunk) (viewport--chunk-weight chunk rows))
      (incf (transcript-viewport--weight viewport) (viewport-chunk-weight chunk))
      (viewport--evict viewport)))
  viewport)

(defgeneric transcript-viewport-replace
    (viewport tag &key text display regions layout-function source-characters)
  (:documentation "Replace the resident chunk identified by TAG."))

(defmethod transcript-viewport-replace ((viewport transcript-viewport) tag
                                        &key text display regions layout-function
                                          (source-characters 0))
  "Replace TAG's chunk at its current width, preserving anchors and follow mode.

Only the replacement is wrapped; other chunks retain their row objects.
LAYOUT-FUNCTION and SOURCE-CHARACTERS have the same meaning as on append.
Return true when TAG matched a resident chunk, NIL otherwise."
  (check-type layout-function (or null function))
  (check-type source-characters (integer 0))
  (let* ((chunks (transcript-viewport--chunks viewport))
         (index  (position tag chunks :key #'viewport-chunk-tag :test #'equal)))
    (when index
      (let* ((number      (+ index (transcript-viewport--chunk-base viewport)))
             (old-chunk   (aref chunks index))
             (new-chunk   (%make-viewport-chunk text display regions tag
                                                layout-function source-characters))
             (rows        (transcript-viewport--rows viewport))
             (start       (or (position number rows :key #'viewport-row-chunk) (length rows)))
             (end         (or (position-if (lambda (row) (> (viewport-row-chunk row) number))
                                          rows :start start)
                              (length rows)))
             (replacement (viewport--wrap-chunk new-chunk number
                                               (transcript-viewport-width viewport)))
             (top         (transcript-viewport-top viewport))
             (anchor      (and top (< top (length rows)) (aref rows top)))
             (delta       (- (length replacement) (- end start)))
             (old-length  (length rows)))
        (setf (viewport-chunk-weight new-chunk)
              (viewport--chunk-weight new-chunk replacement))
        (when (plusp delta)
          (dotimes (offset delta)
            (vector-push-extend nil rows)))
        (replace rows rows :start1 (+ end delta) :start2 end :end2 old-length)
        (replace rows replacement :start1 start)
        (when (minusp delta)
          (viewport--clear-tail rows (+ old-length delta)))
        (setf (aref chunks index) new-chunk)
        (incf (transcript-viewport--weight viewport)
              (- (viewport-chunk-weight new-chunk) (viewport-chunk-weight old-chunk)))
        (when top
          (setf (transcript-viewport-top viewport)
                (cond
                  ((>= top end)
                   (+ top delta))
                  ((and anchor (>= top start))
                   (+ start (or (position-if
                                 (lambda (row)
                                   (> (+ (viewport-row-offset row) (viewport-row-length row))
                                      (viewport-row-offset anchor)))
                                 replacement)
                                (max 0 (1- (length replacement))))))
                  (t
                   top))))
        (viewport--evict viewport)
        t))))

(defun transcript-viewport-resize (viewport width)
  "Reflow VIEWPORT to WIDTH columns, keeping its first visible line in view.

Return true when the width changed."
  (check-type width (integer 1))
  (unless (= width (transcript-viewport-width viewport))
    (let* ((old     (transcript-viewport--rows viewport))
           (top     (transcript-viewport-top viewport))
           (anchor  (and top (< top (length old)) (aref old top)))
           (rows    (make-array 0 :adjustable t :fill-pointer 0))
           (chunks  (make-array 0 :adjustable t :fill-pointer 0))
           (weight  0)
           (new-top nil))
      ;; Publish only after every source callback and wrap succeeds.
      (loop for previous across (transcript-viewport--chunks viewport)
            for index from (transcript-viewport--chunk-base viewport)
            for chunk = (viewport--layout-chunk previous width)
            do (let ((chunk-rows (viewport--wrap-chunk chunk index width)))
                 (dolist (row chunk-rows)
                   (when (and anchor
                              (= index (viewport-row-chunk anchor))
                              (<= (viewport-row-offset row) (viewport-row-offset anchor)))
                     (setf new-top (length rows)))
                   (vector-push-extend row rows))
                 (setf (viewport-chunk-weight chunk) (viewport--chunk-weight chunk chunk-rows))
                 (incf weight (viewport-chunk-weight chunk))
                 (vector-push-extend chunk chunks)))
      (setf (transcript-viewport--chunks viewport) chunks
            (transcript-viewport--rows viewport) rows
            (transcript-viewport--weight viewport) weight
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

;;; Selection

(defun transcript-viewport-selection-start (viewport column row)
  "Begin a selection at the character shown at one-based COLUMN and ROW of the last layout.

Return true when a row is there; otherwise any selection ends."
  (let ((position (viewport--screen-position viewport column row)))
    (setf (transcript-viewport--selection-anchor viewport) position
          (transcript-viewport--selection-head viewport)   position)
    (not (null position))))

(defun transcript-viewport-selection-extend (viewport column row)
  "Stretch the selection to the character shown at one-based COLUMN and ROW of the last layout.

A position above or below the shown rows reaches the first or last of them.
Return true when there is a selection to stretch."
  (when (transcript-viewport--selection-anchor viewport)
    (let ((position (viewport--screen-position viewport column row)))
      (when position
        (setf (transcript-viewport--selection-head viewport) position))
      t)))

(defun transcript-viewport-selection-clear (viewport)
  "End VIEWPORT's selection."
  (setf (transcript-viewport--selection-anchor viewport) nil
        (transcript-viewport--selection-head viewport)   nil)
  (values))

(defun transcript-viewport-selection-text (viewport)
  "Return the plain characters VIEWPORT's selection covers, or NIL without one.

Characters of successive chunks are joined with a newline unless the earlier
chunk ends with one."
  (multiple-value-bind (start end) (viewport--selection-range viewport)
    (when start
      (let ((pieces nil))
        (loop for number from (first start) to (first end)
              for text = (viewport-chunk-text
                          (aref (transcript-viewport--chunks viewport)
                                (- number (transcript-viewport--chunk-base viewport))))
              for from = (if (= number (first start)) (rest start) 0)
              for to = (if (= number (first end)) (rest end) (length text))
              do (when (and pieces
                            (not (zerop (length (first pieces))))
                            (char/= (char (first pieces) (1- (length (first pieces)))) #\Newline))
                   (push (string #\Newline) pieces))
                 (push (subseq text (min from (length text)) (min to (length text))) pieces))
        (let ((text (apply #'concatenate 'string (nreverse pieces))))
          (and (plusp (length text)) text))))))

(defun viewport--screen-position (viewport column row)
  "Return the (CHUNK . INDEX) of the character at one-based COLUMN and ROW, or NIL without rows.

ROW is clamped to the rows the last layout showed and COLUMN past a row's last
cell means the row's end."
  (let ((rows (transcript-viewport--rows viewport)))
    (when (plusp (length rows))
      (let* ((maximum  (transcript-viewport-maximum-top viewport))
             (top      (min (or (transcript-viewport-top viewport) maximum) maximum))
             (last     (min (1- (length rows)) (+ top (transcript-viewport-height viewport) -1)))
             (absolute (max top (min last (+ top (1- row)))))
             (anchor   (aref rows absolute))
             (text     (viewport-chunk-text (viewport--row-chunk viewport anchor)))
             (start    (viewport-row-offset anchor))
             (end      (min (length text) (+ start (viewport-row-length anchor)))))
        (cons (viewport-row-chunk anchor)
              (or (viewport--column-character-index text (max 1 column) :start start :end end)
                  end))))))

(defun viewport--position< (left right)
  "Return true when position LEFT comes before RIGHT."
  (or (< (first left) (first right))
      (and (= (first left) (first right))
           (< (rest left) (rest right)))))

(defun viewport--selection-range (viewport)
  "Return the selection's start and end positions, the end past its last character, or NIL.

Both are clamped to the resident chunks, so eviction and rollback shorten the
selection instead of breaking it."
  (let ((anchor (transcript-viewport--selection-anchor viewport))
        (head   (transcript-viewport--selection-head viewport))
        (base   (transcript-viewport--chunk-base viewport))
        (count  (length (transcript-viewport--chunks viewport))))
    (when (and anchor head (plusp count))
      (let* ((first  (if (viewport--position< head anchor) head anchor))
             (second (if (viewport--position< head anchor) anchor head))
             (last   (+ base count -1))
             (start  (if (< (first first) base) (cons base 0) first))
             (end    (if (> (first second) last)
                         (cons last (length (viewport-chunk-text
                                             (aref (transcript-viewport--chunks viewport) (1- count)))))
                         (cons (first second) (1+ (rest second))))))
        (unless (viewport--position< end start)
          (values start end))))))

(defun viewport--row-selection (viewport row)
  "Return the chunk indexes the selection covers in ROW, as FROM and TO, or NIL."
  (multiple-value-bind (start end) (viewport--selection-range viewport)
    (when start
      (let* ((number (viewport-row-chunk row))
             (offset (viewport-row-offset row))
             (limit  (+ offset (viewport-row-length row)))
             (from   (cond
                           ((< number (first start))
                            nil)
                           ((= number (first start))
                            (max offset (rest start)))
                           (t
                            offset)))
             (to     (cond
                           ((> number (first end))
                            nil)
                           ((= number (first end))
                            (min limit (rest end)))
                           (t
                            limit))))
        (when (and from to (< from to))
          (values from to))))))

(defun viewport--copy-chunk (chunk)
  "Copy mutable state of CHUNK while retaining its immutable contents."
  (let ((copy (%make-viewport-chunk (viewport-chunk-text chunk)
                                    (viewport-chunk-display chunk)
                                    (viewport-chunk-regions chunk)
                                    (viewport-chunk-tag chunk)
                                    (viewport-chunk-layout-function chunk)
                                    (viewport-chunk-source-characters chunk))))
    (setf (viewport-chunk-weight copy) (viewport-chunk-weight chunk))
    copy))

(defun transcript-viewport-checkpoint (viewport)
  "Return an opaque state that TRANSCRIPT-VIEWPORT-ROLLBACK can restore.

The snapshot includes resident chunks and rows, so a failed publication can
undo replacements, evictions, appends, scrolling, and the last layout."
  (list (map 'vector #'viewport--copy-chunk
             (transcript-viewport--chunks viewport))
        (copy-seq (transcript-viewport--rows viewport))
        (transcript-viewport--chunk-base viewport)
        (transcript-viewport--row-base viewport)
        (transcript-viewport--weight viewport)
        (transcript-viewport-width viewport)
        (transcript-viewport-top viewport)
        (transcript-viewport-height viewport)
        (transcript-viewport-maximum-top viewport)))

(defun transcript-viewport-rollback (viewport checkpoint)
  "Restore VIEWPORT to the complete state captured by CHECKPOINT.

Discarded chunks and rows release their storage."
  (destructuring-bind (chunks rows chunk-base row-base weight width top height maximum-top)
      checkpoint
    (setf (transcript-viewport--chunks viewport)
          (make-array (length chunks) :adjustable t :fill-pointer t
                      :initial-contents chunks)
          (transcript-viewport--rows viewport)
          (make-array (length rows) :adjustable t :fill-pointer t
                      :initial-contents rows)
          (transcript-viewport--chunk-base viewport) chunk-base
          (transcript-viewport--row-base viewport) row-base
          (transcript-viewport--weight viewport) weight
          (slot-value viewport 'width) width
          (transcript-viewport-top viewport) top
          (slot-value viewport 'height) height
          (slot-value viewport 'maximum-top) maximum-top))
  viewport)

(defun viewport--row-chunk (viewport row)
  "Return the resident chunk ROW of VIEWPORT shows."
  (aref (transcript-viewport--chunks viewport)
        (- (viewport-row-chunk row) (transcript-viewport--chunk-base viewport))))

(defun viewport--chunk-weight (chunk rows)
  "Return the characters CHUNK and its wrapped ROWS hold, counting each click region."
  (+ (length (viewport-chunk-text chunk))
     (viewport-chunk-source-characters chunk)
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

(defun viewport--layout-chunk (chunk width)
  "Return CHUNK laid out for WIDTH, retaining its source callback and tag."
  (let ((layout (viewport-chunk-layout-function chunk)))
    (if layout
        (multiple-value-bind (text display regions) (funcall layout width)
          (%make-viewport-chunk text display regions (viewport-chunk-tag chunk)
                               layout (viewport-chunk-source-characters chunk)))
        (viewport--copy-chunk chunk))))

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
