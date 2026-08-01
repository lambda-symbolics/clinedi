;;;; -- Terminal rendering --

(in-package #:clinedi)

(defun screen-position (text &key (prompt-width 0) (columns 80) (start 0)
                                  (end (length text)))
  "Return the screen position after TEXT from START through END.

PROMPT-WIDTH is the number of cells preceding TEXT and COLUMNS is the terminal
width. The values are row, zero-based column and whether an exact-width wrap
still needs to be materialized."
  (setf columns (max 1 columns)
        start (min (max 0 start) (length text))
        end (min end (length text)))
  (when (> start end)
    (setf start end))
  (let ((row (floor prompt-width columns))
        (column (mod prompt-width columns))
        (pending-wrap (and (plusp prompt-width)
                           (zerop (mod prompt-width columns))))
        (index start))
    (loop while (< index end)
          do (let ((character (char text index)))
               (cond ((char= character #\newline)
                      (if pending-wrap
                          (setf pending-wrap nil)
                          (incf row))
                      (setf column 0)
                      (incf index))
                     ((char= character #\return)
                      (setf column 0
                            pending-wrap nil)
                      (incf index))
                     (t
                      (let* ((next (grapheme-next-boundary text index end))
                             (width (min columns
                                         (grapheme-cell-width
                                          text index next))))
                        (when (plusp width)
                          (when pending-wrap
                            (setf pending-wrap nil))
                          ;; Wide glyphs wrap as units instead of splitting at
                          ;; the terminal's last column.
                          (when (and (plusp column)
                                     (> (+ column width) columns))
                            (incf row)
                            (setf column 0))
                          (incf column width)
                          (when (= column columns)
                            (incf row)
                            (setf column 0
                                  pending-wrap t)))
                        (setf index next)))))
          finally (return (values row column pending-wrap)))))

(defun screen--row-starts (text columns)
  "Return character indexes beginning TEXT's modeled physical screen rows."
  (let ((starts (list 0))
        (column 0)
        (pending-wrap nil)
        (index 0))
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 ((char= character #\newline)
                  (incf index)
                  (if pending-wrap
                      (setf (first starts) index
                            pending-wrap nil)
                      (push index starts))
                  (setf column 0))
                 ((char= character #\return)
                  (setf column 0
                        pending-wrap nil)
                  (incf index))
                 (t
                  (let* ((next (grapheme-next-boundary text index))
                         (width (min columns
                                     (grapheme-cell-width text index next))))
                    (when (plusp width)
                      (when pending-wrap
                        (setf pending-wrap nil))
                      (when (and (plusp column)
                                 (> (+ column width) columns))
                        (push index starts)
                        (setf column 0))
                      (incf column width)
                      (when (= column columns)
                        (push next starts)
                        (setf column 0
                              pending-wrap t)))
                    (setf index next))))))
    (nreverse starts)))

(defun screen--row-count (text columns start end)
  "Return the physical row count for TEXT between START and END."
  (multiple-value-bind (row column pending-wrap)
      (screen-position text :columns columns :start start :end end)
    (declare (ignore column pending-wrap))
    (1+ row)))

(defun screen--cursor-row-index (starts cursor)
  "Return the last row in STARTS beginning no later than CURSOR."
  (loop for start in starts
        for row from 0
        while (<= start cursor)
        maximize row into cursor-row
        finally (return (or cursor-row 0))))

(defun screen--boundary-positions (text prompt-width columns)
  "Return grapheme boundary indexes and screen positions throughout TEXT."
  (setf columns (max 1 columns)
        prompt-width (max 0 prompt-width))
  (let ((positions nil)
        (row (floor prompt-width columns))
        (column (mod prompt-width columns))
        (pending-wrap (and (plusp prompt-width)
                           (zerop (mod prompt-width columns))))
        (index 0))
    (push (list 0 row column) positions)
    (loop while (< index (length text))
          do (let ((character (char text index)))
               (cond
                 ((char= character #\newline)
                  (incf index)
                  (if pending-wrap
                      (setf pending-wrap nil)
                      (incf row))
                  (setf column 0))
                 ((char= character #\return)
                  (incf index)
                  (setf column 0
                        pending-wrap nil))
                 (t
                  (let* ((next (grapheme-next-boundary text index))
                         (width (min columns
                                     (grapheme-cell-width text index next))))
                    (when (plusp width)
                      (when pending-wrap
                        (setf pending-wrap nil))
                      (when (and (plusp column)
                                 (> (+ column width) columns))
                        (incf row)
                        (setf column 0))
                      (incf column width)
                      (when (= column columns)
                        (incf row)
                        (setf column 0
                              pending-wrap t)))
                    (setf index next))))
             (push (list index row column) positions)))
    (nreverse positions)))

(defstruct (screen-editor-layout
            (:constructor screen--make-editor-layout))
  "A word-wrapped editable display and its source-boundary geometry."
  (display "" :type string)
  (display-indexes #() :type vector)
  (positions nil :type list)
  (soft-breaks nil :type list)
  (end-row 0 :type integer)
  (end-column 0 :type integer)
  (pending-wrap-p nil :type boolean))

(defun screen--word-end (text start end)
  "Return the end of TEXT's word beginning at START, no later than END."
  (loop with index = start
        while (< index end)
        for character = (char text index)
        until (or (char= character #\space)
                  (char= character #\newline)
                  (char= character #\return))
        do (setf index (grapheme-next-boundary text index end))
        finally (return index)))

(defun screen--word-start-p (text index)
  "Return whether INDEX begins a word in TEXT."
  (and (< index (length text))
       (let ((character (char text index)))
         (and (not (char= character #\space))
              (not (char= character #\newline))
              (not (char= character #\return))
              (or (zerop index)
                  (let ((previous (char text (1- index))))
                    (or (char= previous #\space)
                        (char= previous #\newline)
                        (char= previous #\return))))))))

(defun screen--editor-layout
    (text prompt-width columns &key (stable-end (length text)))
  "Return TEXT's word-wrapped editable display and source geometry.

The first display row begins after PROMPT-WIDTH cells and later rows use all
COLUMNS cells. STABLE-END limits words that begin before it, preventing a
transient suffix from reflowing accepted input. Soft wraps preserve source
spaces and insert display-only newlines immediately before the next word."
  (check-type text string)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (and (integerp prompt-width) (not (minusp prompt-width)))
    (error 'type-error :datum prompt-width :expected-type '(integer 0 *)))
  (unless (and (integerp stable-end) (<= 0 stable-end (length text)))
    (error 'type-error
           :datum stable-end
           :expected-type `(integer 0 ,(length text))))
  (let ((display (make-string-output-stream))
        (display-indexes (make-array (1+ (length text)) :initial-element nil))
        (positions nil)
        (soft-breaks nil)
        (display-index 0)
        (row (floor prompt-width columns))
        (column (mod prompt-width columns))
        (pending-wrap-p (and (plusp prompt-width)
                             (zerop (mod prompt-width columns))))
        (index 0))
    (labels ((record-boundary (source-index)
               (setf (aref display-indexes source-index) display-index)
               (push (list source-index row column) positions))

             (advance-newline ()
               (if pending-wrap-p
                   (setf pending-wrap-p nil)
                   (incf row))
               (setf column 0))

             (write-soft-break (source-index)
               (write-char #\newline display)
               (incf display-index)
               (push source-index soft-breaks)
               (advance-newline))

             (word-width (start)
               (let* ((limit (if (< start stable-end)
                                 stable-end
                                 (length text)))
                      (end (screen--word-end text start limit)))
                 (text-cell-width text :start start :end end)))

             (advance-grapheme (start end)
               (let ((width (min columns
                                 (grapheme-cell-width text start end))))
                 (when (plusp width)
                   (when pending-wrap-p
                     (setf pending-wrap-p nil))
                   (when (and (plusp column)
                              (> (+ column width) columns))
                     (incf row)
                     (setf column 0))
                   (incf column width)
                   (when (= column columns)
                     (incf row)
                     (setf column 0
                           pending-wrap-p t))))))
      (when (zerop (length text))
        (record-boundary 0))
      (loop while (< index (length text))
            do (when (and (screen--word-start-p text index)
                          (plusp column)
                          (> (word-width index) (- columns column)))
                 (write-soft-break index))
               (record-boundary index)
               (let ((character (char text index)))
                 (cond ((char= character #\newline)
                        (write-char character display)
                        (incf display-index)
                        (incf index)
                        (advance-newline))
                       ((char= character #\return)
                        (write-char character display)
                        (incf display-index)
                        (incf index)
                        (setf column 0
                              pending-wrap-p nil))
                       (t
                        (let ((next (grapheme-next-boundary
                                     text index (length text))))
                          (write-string text display :start index :end next)
                          (incf display-index (- next index))
                          (advance-grapheme index next)
                          (setf index next))))
               (setf (aref display-indexes index) display-index)
               (push (list index row column) positions)))
      (screen--make-editor-layout
       :display (get-output-stream-string display)
       :display-indexes display-indexes
       :positions (nreverse
                   (remove-duplicates positions :key #'first :from-end t))
       :soft-breaks (nreverse soft-breaks)
       :end-row row
       :end-column column
       :pending-wrap-p pending-wrap-p))))

(defun screen--editor-position (layout source-index)
  "Return LAYOUT's row and column for SOURCE-INDEX."
  (let ((position (find source-index
                        (screen-editor-layout-positions layout)
                        :key #'first)))
    (unless position
      (error 'type-error
             :datum source-index
             :expected-type 'integer))
    (values (second position) (third position))))

(defun render--insert-soft-breaks (text display soft-breaks)
  "Insert SOFT-BREAKS into trusted styled DISPLAY for visible TEXT indexes."
  (unless (string= text (ansi-strip display))
    (error "Highlighted text does not preserve its plain visible content."))
  (with-output-to-string (wrapped)
    (let ((breaks soft-breaks)
          (index 0)
          (visible-index 0))
      (labels ((write-breaks ()
                 (loop while (and breaks
                                  (= (first breaks) visible-index))
                       do (write-char #\newline wrapped)
                          (setf breaks (rest breaks)))))
        (loop while (< index (length display))
              do (write-breaks)
                 (let ((control-end
                         (cl-colorist:ansi-control-end display index)))
                   (if control-end
                       (progn
                         (write-string display wrapped
                                       :start index :end control-end)
                         (setf index control-end))
                       (progn
                         (write-char (char display index) wrapped)
                         (incf index)
                         (incf visible-index)))))
        (write-breaks)))))

(defun wrap-styled-editor-text
    (text display &key (cursor (length text)) (columns 80) (prompt-width 0))
  "Return word-wrapped editable TEXT, styled DISPLAY, and cursor index.

DISPLAY must have exactly TEXT as its ANSI-stripped visible content. CURSOR is
an index in TEXT. PROMPT-WIDTH reserves cells before TEXT on its first row. The
returned strings add display-only newlines before words that no longer fit on
their current row, and the third value is CURSOR's index in the returned plain
string. Words wider than COLUMNS still wrap by grapheme."
  (check-type text string)
  (check-type display string)
  (check-type cursor integer)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (string= text (ansi-strip display))
    (error "Styled editor text does not preserve its plain visible content."))
  (let* ((safe-cursor
           (grapheme-boundary-at-or-after
            text
            (min (length text) (max 0 cursor))))
         (layout (screen--editor-layout text prompt-width columns))
         (wrapped-text (screen-editor-layout-display layout))
         (wrapped-display
           (render--insert-soft-breaks
            text display (screen-editor-layout-soft-breaks layout)))
         (wrapped-cursor
           (aref (screen-editor-layout-display-indexes layout) safe-cursor)))
    (values wrapped-text wrapped-display wrapped-cursor)))

(defun line-editor-move-vertical
    (editor direction &key (columns 80) (prompt-width 0))
  "Move EDITOR by one physical display row and return whether it moved.

DIRECTION is -1 for the previous row or 1 for the next row. COLUMNS and
PROMPT-WIDTH describe the current terminal layout in cells. Repeated movement
retains the original preferred cell column across shorter rows."
  (check-type editor line-editor)
  (unless (member direction '(-1 1))
    (error 'type-error :datum direction :expected-type '(member -1 1)))
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (and (integerp prompt-width) (not (minusp prompt-width)))
    (error 'type-error :datum prompt-width :expected-type '(integer 0 *)))
  (let* ((layout
           (screen--editor-layout
            (line-editor-text editor) prompt-width columns))
         (positions (screen-editor-layout-positions layout))
         (cursor-position
           (find (line-editor-cursor editor) positions :key #'first))
         (target-row (+ (second cursor-position) direction))
         (preferred-column
           (or (slot-value editor 'vertical-column)
               (third cursor-position)))
         (best nil)
         (best-distance nil))
    (dolist (position positions)
      (when (= (second position) target-row)
        (let ((distance (abs (- preferred-column (third position)))))
          (when (or (null best)
                    (< distance best-distance)
                    (and (= distance best-distance)
                         (< (third position) (third best))))
            (setf best position
                  best-distance distance)))))
    (when best
      (setf (slot-value editor 'cursor) (first best)
            (slot-value editor 'vertical-column) preferred-column)
      t)))

(defun screen-window
    (text &key (cursor (length text)) (columns 80) (rows 24))
  "Return a cursor-containing character window of TEXT fitting within ROWS.

The values are start index, end index, cursor index relative to the window,
whether content precedes the window, and whether content follows it. Indexes
are extended-grapheme boundaries."
  (check-type text string)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (and (integerp rows) (plusp rows))
    (error 'type-error :datum rows :expected-type '(integer 1 *)))
  (check-type cursor integer)
  (let* ((safe-cursor
           (grapheme-boundary-at-or-after
            text
            (min (length text) (max 0 cursor))))
         (starts (screen--row-starts text columns))
         (total-rows (length starts)))
    (when (<= total-rows rows)
      (return-from screen-window
        (values 0 (length text) safe-cursor nil nil)))
    (let* ((content-rows (max 1 (1- rows)))
           (cursor-row (screen--cursor-row-index starts safe-cursor))
           (start-row
             (min (max 0 (- cursor-row (floor content-rows 2)))
                  (max 0 (- total-rows content-rows))))
           (end-row (min total-rows (+ start-row content-rows)))
           (start (nth start-row starts))
           (end (if (< end-row total-rows)
                    (nth end-row starts)
                    (length text))))
      (setf start (min start safe-cursor)
            end (max end safe-cursor))
      (loop while (> (screen--row-count text columns start end) rows)
            do (cond
                 ((> end safe-cursor)
                  (setf end (grapheme-previous-boundary text end)))
                 ((< start safe-cursor)
                  (setf start (grapheme-next-boundary
                               text start safe-cursor)))
                 (t
                  (return))))
      (values start
              end
              (- safe-cursor start)
              (plusp start)
              (< end (length text))))))

(defun write-display (text &key (stream *standard-output*))
  "Write TEXT to STREAM, following newlines with explicit returns."
  (loop for character across text
        do (write-char character stream)
        when (char= character #\newline)
          do (write-char #\return stream))
  (values))

(defun render--write-newline (stream)
  "Write a terminal newline without relying on output post-processing."
  (write-char #\linefeed stream)
  (write-char #\return stream)
  (values))

(defun render--overwrite-display (text stream)
  "Overwrite display TEXT, clearing stale cells before explicit newlines."
  (loop for character across text
        do (if (char= character #\newline)
               (progn
                 (write-string (ansi-clear-line-right) stream)
                 (render--write-newline stream))
               (write-char character stream)))
  (values))

(defun render--write-prompt (prompt prompt-width columns stream)
  "Write the static editable PROMPT and return its ending row."
  (write-string prompt stream)
  (multiple-value-bind (row column pending-wrap)
      (screen-position "" :prompt-width prompt-width :columns columns)
    (declare (ignore column))
    (when pending-wrap
      (render--write-newline stream))
    (force-output stream)
    row))

(defun render-line (text &key (cursor (length text))
                              (prompt-width 0)
                              (columns 80)
                              (previous-row 0)
                              suggestion
                              footer-text
                              footer-display
                              (highlight-function #'identity)
                              (stream *standard-output*))
  "Redraw dynamic line-editor TEXT and return the cursor's resulting row.

The static prompt is described by PROMPT-WIDTH but is not repainted. CURSOR is
a character index. PREVIOUS-ROW is the row where the prior redraw left it.
SUGGESTION is unaccepted suffix text rendered after TEXT. HIGHLIGHT-FUNCTION
may add ANSI styling, but must preserve the visible content of TEXT.
FOOTER-TEXT and FOOTER-DISPLAY describe optional plain geometry and trusted
ANSI presentation below the editor. Their visible contents must match."
  (let* ((columns (max 1 columns))
         (safe-cursor
           (grapheme-boundary-at-or-after
            text
            (min (length text) (max 0 cursor))))
         (suffix (or suggestion ""))
         (footer (or footer-text ""))
         (footer-p (plusp (length footer)))
         (accepted-layout
           (screen--editor-layout text prompt-width columns))
         (combined-text (concatenate 'string text suffix))
         (combined-layout
           (screen--editor-layout combined-text prompt-width columns
                                  :stable-end (length text)))
         (combined-display (screen-editor-layout-display combined-layout))
         (display (concatenate 'string
                               combined-display
                               (if footer-p (string #\newline) "")
                               footer))
         (highlighted (funcall highlight-function text))
         (presented
           (render--insert-soft-breaks
            combined-text
            (concatenate 'string
                         highlighted
                         (ansi-colorize suffix :bright-black))
            (screen-editor-layout-soft-breaks combined-layout))))
    (multiple-value-bind (prompt-row prompt-column prompt-wrap)
        (screen-position "" :prompt-width prompt-width :columns columns)
      (declare (ignore prompt-wrap))
      (multiple-value-bind (target-row target-column)
          (screen--editor-position accepted-layout safe-cursor)
        (multiple-value-bind (end-row end-column exact-wrap)
            (screen-position display :prompt-width prompt-width
                                     :columns columns)
          (declare (ignore end-column))
          (write-string (ansi-cursor-hide) stream)
          (unwind-protect
               (progn
                 (write-string (ansi-cursor-up (- previous-row prompt-row))
                               stream)
                 (write-string (ansi-cursor-column prompt-column) stream)
                 (render--overwrite-display presented stream)
                 (when footer-p
                   (write-string (ansi-clear-line-right) stream)
                   (render--write-newline stream)
                   (render--overwrite-display
                    (or footer-display footer) stream))
                 (when exact-wrap
                   (render--write-newline stream))
                 (write-string (ansi-clear-below) stream)
                 (write-string (ansi-cursor-up (- end-row target-row)) stream)
                 (write-string (ansi-cursor-column target-column) stream))
            (write-string (ansi-cursor-show) stream)
            (force-output stream))
          target-row)))))

(defun render--finish-line
    (text &key (prompt-width 0) (columns 80) (previous-row 0) marker
               (highlight-function #'identity) (stream *standard-output*))
  "Park after TEXT, optionally print MARKER, and advance to a fresh line."
  (render-line text
               :cursor (length text)
               :prompt-width prompt-width
               :columns columns
               :previous-row previous-row
               :highlight-function highlight-function
               :stream stream)
  (when marker
    (write-string (ansi-colorize marker :bright-black) stream))
  (render--write-newline stream)
  (force-output stream)
  (values))

(defun print-candidates (displays &key (columns 80)
                                       (stream *standard-output*))
  "Print completion DISPLAYS in terminal-cell-aligned columns."
  (let ((count (length displays)))
    (cond ((zerop count)
           nil)
          ((> count 120)
           (format stream "(~d possibilities)" count)
           (render--write-newline stream))
          (t
           (let* ((width (+ 2 (loop for display in displays
                                    maximize (text-cell-width display))))
                  (per-row (max 1 (floor (max 1 columns) (max 1 width)))))
             (loop for index from 1
                   for display in displays
                   for display-width = (text-cell-width display)
                   do (write-string display stream)
                      (loop repeat (- width display-width)
                            do (write-char #\space stream))
                   when (zerop (mod index per-row))
                     do (render--write-newline stream)
                   finally
                      (unless (zerop (mod count per-row))
                        (render--write-newline stream)))))))
  (force-output stream)
  (values))

(defun split-prompt (prompt)
  "Split PROMPT into preamble and final editable line values."
  (let ((break (position #\newline prompt :from-end t)))
    (if break
        (values (subseq prompt 0 (1+ break)) (subseq prompt (1+ break)))
        (values "" prompt))))
