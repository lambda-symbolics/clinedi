;;;; -- Scrollback-safe live regions --

(in-package #:clinedi)

(defclass live-region ()
  ((write-function
    :initarg :write-function
    :reader live-region--write-function
    :type function
    :documentation "Function accepting one trusted terminal output string.")
   (flush-function
    :initarg :flush-function
    :reader live-region--flush-function
    :type function
    :documentation "Function making pending terminal output visible.")
   (columns
    :initarg :columns
    :accessor live-region-columns
    :type integer
    :documentation "Positive terminal width used for live-region geometry.")
   (maximum-rows
    :initarg :maximum-rows
    :initform nil
    :accessor live-region-maximum-rows
    :type (or null integer)
    :documentation "Optional positive cap on painted physical terminal rows.")
   (text
    :initform ""
    :accessor live-region--text
    :type string
    :documentation "Plain visible content of the most recent presentation.")
   (display
    :initform ""
    :accessor live-region--display
    :type string
    :documentation "Trusted styled content of the most recent presentation.")
   (cursor
    :initform 0
    :accessor live-region--cursor
    :type integer
    :documentation "Character index of the desired cursor within TEXT.")
   (presented-p
    :initform nil
    :accessor live-region--presented-p
    :type boolean
    :documentation "Whether the region retains content that can be repainted.")
   (visible-p
    :initform nil
    :accessor live-region-visible-p
    :type boolean
    :documentation "Whether the retained presentation is currently painted.")
   (row-count
    :initform 0
    :accessor live-region-row-count
    :type integer
    :documentation "Number of currently painted physical terminal rows.")
   (cursor-row
    :initform 0
    :accessor live-region-cursor-row
    :type integer
    :documentation "Zero-based live row holding the painted cursor.")
   (cursor-column
    :initform 0
    :accessor live-region-cursor-column
    :type integer
    :documentation "Zero-based terminal column holding the painted cursor.")
   (cursor-visible-p
    :initform t
    :accessor live-region-cursor-visible-p
    :type boolean
    :documentation "Whether terminal updates leave the physical cursor visible."))
  (:documentation
   "Transient terminal content that remains beneath ordinary scrollback output."))


;;;; -- Construction and Validation --

(defun live-region--standard-write (text)
  "Write trusted terminal TEXT to the current standard output."
  (write-string text *standard-output*)
  (values))

(defun live-region--standard-flush ()
  "Flush the current standard output."
  (force-output *standard-output*)
  (values))

(defun make-live-region (&key (columns 80) maximum-rows
                              (write-function #'live-region--standard-write)
                              (flush-function #'live-region--standard-flush))
  "Create a live region using COLUMNS, optional MAXIMUM-ROWS, and callbacks."
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (or (null maximum-rows)
              (and (integerp maximum-rows) (plusp maximum-rows)))
    (error 'type-error :datum maximum-rows
                       :expected-type '(or null (integer 1 *))))
  (check-type write-function function)
  (check-type flush-function function)
  (make-instance 'live-region
                 :columns columns
                 :maximum-rows maximum-rows
                 :write-function write-function
                 :flush-function flush-function))

(defun live-region--geometry-text-p (text)
  "True when TEXT contains only printable characters and modeled line breaks."
  (loop for character across text
        for code = (char-code character)
        always (or (char= character #\newline)
                   (char= character #\return)
                   (and (>= code 32)
                        (not (<= 127 code 159))))))

(defun live-region--validate-display (text display)
  "Require TEXT and styled DISPLAY to have identical safe visible content."
  (check-type text string)
  (check-type display string)
  (unless (live-region--geometry-text-p text)
    (error "Live-region geometry text contains an unmodeled terminal control."))
  (unless (string= text (ansi-strip display))
    (error 'styled-text-error :text text :display display))
  (values))


;;;; -- Terminal Output --

(defun live-region--write (region text)
  "Write trusted TEXT through REGION's terminal callback."
  (funcall (live-region--write-function region) text)
  (values))

(defun live-region--flush (region)
  "Make REGION's pending terminal output visible."
  (funcall (live-region--flush-function region))
  (values))

(defun live-region--write-newline (stream)
  "Advance STREAM's terminal to column zero on a fresh row."
  (write-char #\linefeed stream)
  (write-char #\return stream)
  (values))

(defun live-region--emit-update (region function)
  "Emit one terminal update composed by FUNCTION with motion hidden."
  (let ((stream (make-string-output-stream)))
    (write-string (ansi-cursor-hide) stream)
    (funcall function stream)
    (write-string (if (live-region-cursor-visible-p region)
                      (ansi-cursor-show)
                      (ansi-cursor-hide))
                  stream)
    (live-region--write region (get-output-stream-string stream))
    (live-region--flush region))
  (values))

(defun live-region--write-erasure (region stream)
  "Write the commands retracting REGION's painted rows to STREAM."
  (let ((total (live-region-row-count region)))
    (write-string
     (ansi-cursor-down
      (- total 1 (live-region-cursor-row region)))
     stream)
    (loop for row downfrom (1- total) to 0
          do (write-char #\return stream)
             (write-string (ansi-clear-line-right) stream)
             (when (plusp row)
               (write-string (ansi-cursor-up 1) stream))))
  (values))

(defun live-region--windowed-presentation (region)
  "Return REGION's retained presentation cropped around its cursor when needed."
  (let ((text (live-region--text region))
        (display (live-region--display region))
        (cursor (live-region--cursor region))
        (maximum-rows (live-region-maximum-rows region)))
    (if maximum-rows
        (multiple-value-bind (start end window-cursor before-p after-p)
            (screen-window text
                           :cursor cursor
                           :columns (live-region-columns region)
                           :rows maximum-rows)
          (declare (ignore before-p after-p))
          (values (subseq text start end)
                  (ansi--visible-slice display start end)
                  window-cursor))
        (values text display cursor))))

(defun live-region--text-geometry (text cursor columns)
  "Return end and cursor geometry for TEXT at COLUMNS."
  (multiple-value-bind (cursor-row cursor-column cursor-wrap)
      (screen-position text :columns columns :end cursor)
    (declare (ignore cursor-wrap))
    (multiple-value-bind (end-row end-column pending-wrap)
        (screen-position text :columns columns)
      (declare (ignore end-column))
      (values end-row cursor-row cursor-column pending-wrap))))

(defun live-region--presentation-geometry (region text cursor)
  "Return end and cursor geometry for REGION's windowed TEXT and CURSOR."
  (live-region--text-geometry
   text cursor (live-region-columns region)))

(defun live-region--write-presentation
    (stream display &key end-row cursor-row cursor-column pending-wrap)
  "Write DISPLAY and its logical cursor placement to STREAM."
  (write-display display :stream stream)
  (when pending-wrap
    (live-region--write-newline stream))
  (write-string (ansi-cursor-up (- end-row cursor-row)) stream)
  (write-string (ansi-cursor-column cursor-column) stream)
  (values))

(defun live-region--record-presentation
    (region &key end-row cursor-row cursor-column)
  "Record REGION's painted END-ROW and cursor geometry."
  (setf (live-region-row-count region) (1+ end-row)
        (live-region-cursor-row region) cursor-row
        (live-region-cursor-column region) cursor-column
        (live-region-visible-p region) t)
  region)

(defun live-region--record-reflowed-presentation (region columns)
  "Record REGION's painted presentation after terminal reflow to COLUMNS."
  (multiple-value-bind (text display cursor)
      (live-region--windowed-presentation region)
    (declare (ignore display))
    (multiple-value-bind (end-row cursor-row cursor-column pending-wrap)
        (live-region--text-geometry text cursor columns)
      (declare (ignore pending-wrap))
      (live-region--record-presentation region
                                        :end-row       end-row
                                        :cursor-row    cursor-row
                                        :cursor-column cursor-column))))

(defun live-region--paint (region)
  "Paint REGION's retained presentation and restore its logical cursor."
  (unless (live-region--presented-p region)
    (return-from live-region--paint region))
  (multiple-value-bind (text display cursor)
      (live-region--windowed-presentation region)
    (multiple-value-bind (end-row cursor-row cursor-column pending-wrap)
        (live-region--presentation-geometry region text cursor)
      (live-region--emit-update
       region
       (lambda (stream)
         (live-region--write-presentation stream display
                                          :end-row       end-row
                                          :cursor-row    cursor-row
                                          :cursor-column cursor-column
                                          :pending-wrap  pending-wrap)))
      (live-region--record-presentation region
                                        :end-row       end-row
                                        :cursor-row    cursor-row
                                        :cursor-column cursor-column)))
  region)

(defun live-region--replace
    (region text &key display cursor (appended-text "") (appended-display ""))
  "Replace REGION, atomically writing optional appended scrollback first."
  (setf (live-region--text region) (copy-seq text)
        (live-region--display region) (copy-seq display)
        (live-region--cursor region) cursor
        (live-region--presented-p region) t)
  (multiple-value-bind (window-text window-display window-cursor)
      (live-region--windowed-presentation region)
    (multiple-value-bind (end-row cursor-row cursor-column pending-wrap)
        (live-region--presentation-geometry
         region window-text window-cursor)
      (live-region--emit-update
       region
       (lambda (stream)
         (when (live-region-visible-p region)
           (live-region--write-erasure region stream))
         (when (plusp (length appended-text))
           (write-display appended-display :stream stream)
           (unless (char= (char appended-text (1- (length appended-text)))
                          #\newline)
             (live-region--write-newline stream)))
         (live-region--write-presentation stream window-display
                                          :end-row       end-row
                                          :cursor-row    cursor-row
                                          :cursor-column cursor-column
                                          :pending-wrap  pending-wrap)))
      (live-region--record-presentation region
                                        :end-row       end-row
                                        :cursor-row    cursor-row
                                        :cursor-column cursor-column)))
  region)


;;;; -- Public Lifecycle --

(defun live-region-set-cursor-visible (region visible-p)
  "Set whether REGION leaves the terminal cursor visible and apply it now.

Repaints always hide cursor motion. When visibility is disabled, every repaint
also leaves the cursor hidden until visibility is enabled or REGION is
dismissed."
  (check-type region live-region)
  (check-type visible-p boolean)
  (unless (eq visible-p (live-region-cursor-visible-p region))
    (setf (live-region-cursor-visible-p region) visible-p)
    (live-region--write region
                        (if visible-p
                            (ansi-cursor-show)
                            (ansi-cursor-hide)))
    (live-region--flush region))
  region)

(defun live-region-suspend (region)
  "Retract REGION without forgetting the presentation that should resume."
  (check-type region live-region)
  (when (live-region-visible-p region)
    (live-region--emit-update
     region
     (lambda (stream)
       (live-region--write-erasure region stream)))
    (setf (live-region-visible-p region) nil
          (live-region-row-count region) 0
          (live-region-cursor-row region) 0
          (live-region-cursor-column region) 0))
  region)

(defun live-region-resume (region)
  "Repaint REGION's retained presentation when it is currently retracted."
  (check-type region live-region)
  (when (and (live-region--presented-p region)
             (not (live-region-visible-p region)))
    (live-region--paint region))
  region)

(defun call-with-live-region-suspended (region function)
  "Call FUNCTION while REGION is retracted, then restore it without cursor flashes.

When REGION normally shows the cursor, compound output keeps it hidden from the
initial retraction through the final repaint and reveals it once at the end."
  (check-type region live-region)
  (check-type function function)
  (let ((visible-p (live-region-visible-p region))
        (cursor-visible-p (live-region-cursor-visible-p region)))
    (when cursor-visible-p
      (setf (live-region-cursor-visible-p region) nil)
      (unless visible-p
        (live-region--write region (ansi-cursor-hide))
        (live-region--flush region)))
    (unwind-protect
         (progn
           (when visible-p
             (live-region-suspend region))
           (funcall function))
      (unwind-protect
           (when visible-p
             (live-region-resume region))
        (setf (live-region-cursor-visible-p region) cursor-visible-p)
        (when cursor-visible-p
          (live-region--write region (ansi-cursor-show))
          (live-region--flush region))))))

(defmacro with-live-region-suspended ((region) &body body)
  "Evaluate BODY with REGION retracted, restoring it after every exit."
  `(call-with-live-region-suspended ,region (lambda () ,@body)))

(defun live-region-present (region text &key (cursor (length text))
                                             (display text))
  "Replace REGION with TEXT and trusted styled DISPLAY, placing its cursor.

DISPLAY may contain trusted ANSI styling but must have TEXT as its exact visible
content. CURSOR is normalized to a complete grapheme boundary within TEXT."
  (check-type region live-region)
  (live-region--validate-display text display)
  (check-type cursor integer)
  (let ((safe-cursor
          (grapheme-boundary-at-or-after
           text
           (min (length text) (max 0 cursor)))))
    (live-region--replace region text :display display :cursor safe-cursor)))

(defun live-region-append-and-present
    (region appended-text text
     &key (appended-display appended-text)
          (cursor (length text))
          (display text))
  "Append scrollback and replace REGION in one terminal write and flush.

APPENDED-DISPLAY and DISPLAY may contain trusted ANSI styling but must preserve
the visible contents of APPENDED-TEXT and TEXT. A missing newline after
APPENDED-TEXT is supplied before the replacement presentation. CURSOR is
normalized to a complete grapheme boundary within TEXT."
  (check-type region live-region)
  (live-region--validate-display appended-text appended-display)
  (live-region--validate-display text display)
  (check-type cursor integer)
  (let ((safe-cursor
          (grapheme-boundary-at-or-after
           text
           (min (length text) (max 0 cursor)))))
    (live-region--replace region text
                          :display          display
                          :cursor           safe-cursor
                          :appended-text    appended-text
                          :appended-display appended-display)))

(defun live-region-append (region text &key (display text))
  "Append TEXT to ordinary scrollback and repaint REGION beneath it.

DISPLAY may add trusted ANSI styling while preserving TEXT's visible content.
A missing final newline is supplied so the repainted region never shares the
last output row."
  (check-type region live-region)
  (live-region--validate-display text display)
  (when (plusp (length text))
    (call-with-live-region-suspended
     region
     (lambda ()
       (live-region--write
        region
        (with-output-to-string (stream)
          (write-display display :stream stream)))
       (unless (char= (char text (1- (length text))) #\newline)
         (live-region--write region (format nil "~c~c"
                                            #\linefeed #\return)))
       (live-region--flush region))))
  region)

(defun live-region-dismiss (region)
  "Retract REGION, forget its presentation, and restore cursor visibility."
  (check-type region live-region)
  (live-region-suspend region)
  (setf (live-region--text region) ""
        (live-region--display region) ""
        (live-region--cursor region) 0
        (live-region--presented-p region) nil)
  (live-region-set-cursor-visible region t)
  region)

(defun live-region-resize
    (region columns &key (maximum-rows nil maximum-rows-p) (repaint-p t))
  "Reflow REGION for COLUMNS and optional MAXIMUM-ROWS without replaying output.

When REPAINT-P is false, record the terminal's reflowed geometry but leave the
painted content in place. A following presentation can then replace it in one
atomic frame."
  (check-type region live-region)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (when maximum-rows-p
    (unless (or (null maximum-rows)
                (and (integerp maximum-rows) (plusp maximum-rows)))
      (error 'type-error :datum maximum-rows
                         :expected-type '(or null (integer 1 *)))))
  (check-type repaint-p boolean)
  (unless (and (= columns (live-region-columns region))
               (or (not maximum-rows-p)
                   (eql maximum-rows (live-region-maximum-rows region))))
    (let ((visible-p (live-region-visible-p region))
          (text (live-region--text region))
          (display (live-region--display region))
          (cursor (live-region--cursor region)))
      (when visible-p
        (live-region--record-reflowed-presentation region columns))
      (setf (live-region-columns region) columns)
      (when maximum-rows-p
        (setf (live-region-maximum-rows region) maximum-rows))
      (when (and visible-p repaint-p)
        (live-region--replace region text :display display :cursor cursor))))
  region)
