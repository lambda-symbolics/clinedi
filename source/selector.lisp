;;;; -- Candidate Selection --

(in-package #:clinedi)

(defclass selector ()
  ((items
    :initarg :items
    :reader selector-items
    :type list
    :documentation "Candidate values in presentation order.")
   (selection
    :initform 0
    :reader selector-selection
    :type integer
    :documentation "Zero-based selected candidate index.")
   (visible-count
    :initarg :visible-count
    :reader selector-visible-count
    :type integer
    :documentation "Maximum number of candidates in the visible window.")
   (arrangement
    :initarg :arrangement
    :reader selector-arrangement
    :type keyword
    :documentation "Candidate arrangement, either :VERTICAL or :GRID.")
   (column-count
    :initform 1
    :reader selector-column-count
    :type integer
    :documentation "Column count established by the latest arrangement."))
  (:documentation
   "Application-neutral candidate selection and viewport state.

Candidate values remain opaque to Clinedi. Applications own their labels,
descriptions, filtering, styling and accepted-value semantics."))

(defun selector--copy-items (items)
  "Return a fresh list containing the candidate values from ITEMS."
  (unless (typep items 'sequence)
    (error 'type-error :datum items :expected-type 'sequence))
  (loop for item across (coerce items 'vector)
        collect item))

(defun make-selector (&key (items nil) (visible-count 6)
                           (arrangement :vertical))
  "Create candidate selection state for ITEMS.

VISIBLE-COUNT must be positive. The sequence container is copied, while its
opaque candidate values are retained. ARRANGEMENT is :VERTICAL for one item
per row or :GRID for as many measured columns as the available width permits.
An empty selector has no selected item."
  (unless (and (integerp visible-count) (plusp visible-count))
    (error 'type-error
           :datum visible-count
           :expected-type '(integer 1 *)))
  (unless (member arrangement '(:vertical :grid))
    (error 'type-error
           :datum arrangement
           :expected-type '(member :vertical :grid)))
  (make-instance 'selector
                 :items (selector--copy-items items)
                 :visible-count visible-count
                 :arrangement arrangement))

(defun selector-set-items (selector items &key (test #'equal))
  "Replace SELECTOR candidates with ITEMS and return SELECTOR.

Selection returns to the first candidate when the candidate sequence changes
according to TEST. Reinstalling an equal sequence preserves navigation state."
  (check-type selector selector)
  (let ((new-items (selector--copy-items items)))
    (unless (and (= (length new-items) (length (selector-items selector)))
                 (every test new-items (selector-items selector)))
      (setf (slot-value selector 'selection) 0))
    (setf (slot-value selector 'items) new-items))
  selector)

(defun selector-selected-item (selector)
  "Return SELECTOR's selected candidate, or NIL when it is empty."
  (check-type selector selector)
  (let ((items (selector-items selector)))
    (and items
         (nth (min (selector-selection selector)
                   (1- (length items)))
              items))))

(defun selector-move (selector offset)
  "Move SELECTOR by signed OFFSET with wraparound and return SELECTOR."
  (check-type selector selector)
  (check-type offset integer)
  (let ((count (length (selector-items selector))))
    (when (plusp count)
      (setf (slot-value selector 'selection)
            (mod (+ (selector-selection selector) offset) count))))
  selector)

(defun selector-window (selector)
  "Return SELECTOR's visible candidates, start index and local selection.

The first value is a fresh candidate list. The second is its start index in
SELECTOR-ITEMS. The third is the selected index relative to that list, or NIL
when the selector is empty. The window follows the selection toward either
end and never exceeds SELECTOR-VISIBLE-COUNT."
  (check-type selector selector)
  (let* ((items (selector-items selector))
         (count (length items)))
    (if (zerop count)
        (values nil 0 nil)
        (let* ((selection (min (selector-selection selector) (1- count)))
               (visible-count (min (selector-visible-count selector) count))
               (column-count (selector-column-count selector))
               (raw-start
                 (min (max 0 (- (1+ selection) visible-count))
                      (- count visible-count)))
               (start
                 (* (floor raw-start column-count) column-count)))
          (loop while (>= selection (+ start visible-count))
                do (incf start column-count))
          (values (subseq items start (min count (+ start visible-count)))
                  start
                  (- selection start))))))

(defun selector--column-widths (widths column-count)
  "Return maximum WIDTHS for each row-major column in COLUMN-COUNT."
  (let ((column-widths (make-array column-count :initial-element 0)))
    (loop for width in widths
          for index from 0
          for column = (mod index column-count)
          do (setf (aref column-widths column)
                   (max (aref column-widths column) width)))
    (coerce column-widths 'list)))

(defun selector--fitting-column-count (selector widths &key columns column-gap)
  "Return the widest permitted column count fitting WIDTHS in COLUMNS."
  (if (eq (selector-arrangement selector) :vertical)
      1
      (loop for column-count downfrom (length widths) to 1
            for column-widths = (selector--column-widths
                                 widths column-count)
            for total-width = (+ (reduce #'+ column-widths :initial-value 0)
                                 (* column-gap (1- column-count)))
            when (or (= column-count 1)
                     (<= total-width columns))
              return column-count)))

(defun selector-arrange
    (selector columns &key (width-function (lambda (item)
                                             (text-cell-width
                                              (princ-to-string item))))
                           (column-gap 2))
  "Arrange SELECTOR for COLUMNS and return index rows and column widths.

WIDTH-FUNCTION returns the terminal-cell width of an opaque candidate.
COLUMN-GAP is the number of cells reserved between grid columns. Vertical
selectors always return one index per row. Grid selectors choose the greatest
row-major column count whose measured columns fit. The returned indexes refer
to SELECTOR-ITEMS, and the second value contains each column's measured width."
  (check-type selector selector)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (and (integerp column-gap) (not (minusp column-gap)))
    (error 'type-error :datum column-gap :expected-type '(integer 0 *)))
  (multiple-value-bind (visible-items start local-selection)
      (selector-window selector)
    (declare (ignore local-selection))
    (if (null visible-items)
        (progn
          (setf (slot-value selector 'column-count) 1)
          (values nil nil))
        (let ((widths
                (loop for item in visible-items
                      for width = (funcall width-function item)
                      do (unless (and (integerp width) (not (minusp width)))
                           (error 'type-error
                                  :datum width
                                  :expected-type '(integer 0 *)))
                      collect width)))
          (let* ((column-count
                   (selector--fitting-column-count selector widths
                                                   :columns    columns
                                                   :column-gap column-gap))
                 (column-widths
                   (selector--column-widths widths column-count)))
            (setf (slot-value selector 'column-count) column-count)
            (values
             (loop for offset from 0 below (length visible-items)
                               by column-count
                   collect
                   (loop for local-index from offset
                                           below (min (length visible-items)
                                                      (+ offset column-count))
                         collect (+ start local-index)))
             column-widths))))))

(defun selector--move-row (selector offset)
  "Move SELECTOR by signed row OFFSET using its arranged column count."
  (let* ((items (selector-items selector))
         (count (length items))
         (columns (selector-column-count selector)))
    (when (plusp count)
      (let* ((selection (selector-selection selector))
             (row-count (ceiling count columns))
             (row (floor selection columns))
             (column (mod selection columns))
             (target-row (mod (+ row offset) row-count))
             (target (min (+ (* target-row columns) column)
                          (1- count))))
        (setf (slot-value selector 'selection) target))))
  selector)

(defun selector-handle-event (selector event)
  "Apply navigation EVENT and return an action and optional selected value.

Arrow events navigate with wraparound using the latest arranged grid. Tab and
Shift-Tab cycle linearly forward and backward. Submission accepts. Escape,
interruption and end-of-input cancel. Other input dismisses selection with the
selected value, allowing an editor to retain that candidate before applying
the original event."
  (check-type selector selector)
  (cond
    ((member event '(:up :history-previous))
     (selector--move-row selector -1)
     (values :changed (selector-selected-item selector)))
    ((member event '(:down :history-next))
     (selector--move-row selector 1)
     (values :changed (selector-selected-item selector)))
    ((eq event :left)
     (when (> (selector-column-count selector) 1)
       (selector-move selector -1))
     (values :changed (selector-selected-item selector)))
    ((eq event :right)
     (when (> (selector-column-count selector) 1)
       (selector-move selector 1))
     (values :changed (selector-selected-item selector)))
    ((eq event :complete)
     (selector-move selector 1)
     (values :changed (selector-selected-item selector)))
    ((eq event :complete-previous)
     (selector-move selector -1)
     (values :changed (selector-selected-item selector)))
    ((eq event :submit)
     (if (selector-items selector)
         (values :accept (selector-selected-item selector))
         (values :unhandled nil)))
    ((member event '(:escape :interrupt :end-of-input :stream-end))
     (values :cancel nil))
    ((selector-items selector)
     (values :dismiss (selector-selected-item selector)))
    (t
     (values :unhandled nil))))
