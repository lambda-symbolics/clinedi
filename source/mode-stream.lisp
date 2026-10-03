;;;; -- Terminal mode tracking --

(in-package #:clinedi)

(defparameter *mode-tracking-private-modes*
  '((1049 :alternate-screen nil)
    (1000 :mouse-reporting nil)
    (1006 :sgr-mouse-encoding nil)
    (2004 :bracketed-paste nil)
    (25 :cursor-visible t)
    (7 :autowrap t))
  "Tracked DEC private modes as (NUMBER NAME DEFAULT-SET-P).

The order is the restoring order, so the alternate screen is left first and
the normal screen then receives the remaining resets.")

(defparameter *mode-tracking-parameter-limit* 32
  "Longest private-mode parameter text collected before the control is ignored.")

(defclass mode-tracking-output-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((output
    :initarg :output
    :reader mode-tracking-output-stream-output
    :documentation "The destination receiving every character unchanged.")
   (state
    :initform :ground
    :accessor mode-tracking-output-stream--state
    :documentation "The control parser state: :GROUND, :ESCAPE, :CSI, :PRIVATE or :OSC.")
   (parameters
    :initform (make-string-output-stream)
    :accessor mode-tracking-output-stream--parameters
    :documentation "Parameter characters of the control being parsed.")
   (parameter-count
    :initform 0
    :accessor mode-tracking-output-stream--parameter-count
    :documentation "Characters collected into PARAMETERS.")
   (imposed
    :initform nil
    :accessor mode-tracking-output-stream--imposed
    :documentation "Names of the modes the output currently holds away from their defaults."))
  (:documentation
   "Forward output while tracking the terminal modes it imposes.

Alternate screen, mouse reporting, SGR mouse encoding, bracketed paste, cursor
visibility, autowrap and OSC 10 and 11 default colors are recognized even when
a control arrives split across several writes. Nothing but parser state is
retained, so arbitrary output can pass through."))

(defun make-mode-tracking-output-stream (output)
  "Return a stream forwarding to OUTPUT while tracking the modes it imposes."
  (make-instance 'mode-tracking-output-stream :output output))

(defun mode-tracking-output-stream-imposed-modes (stream)
  "Return the names of the modes STREAM's output holds away from their defaults.

Names are :ALTERNATE-SCREEN, :MOUSE-REPORTING, :SGR-MOUSE-ENCODING,
:BRACKETED-PASTE, :CURSOR-VISIBLE (hidden), :AUTOWRAP (disabled),
:DEFAULT-FOREGROUND and :DEFAULT-BACKGROUND."
  (copy-list (mode-tracking-output-stream--imposed stream)))

(defun mode-tracking-output-stream-restore (stream)
  "Write the controls returning every imposed mode to its default, then flush.

Return the restored mode names, or NIL when nothing was imposed. Restoring is
idempotent, and the stream keeps tracking later output."
  (let ((imposed (mode-tracking-output-stream--imposed stream))
        (output (mode-tracking-output-stream-output stream)))
    (when imposed
      (setf (mode-tracking-output-stream--imposed stream) nil)
      (write-string (mode-tracking--restore-sequence imposed) output)
      (finish-output output)
      imposed)))

(defun mode-tracking--restore-sequence (imposed)
  "Return the controls returning the IMPOSED modes to their defaults."
  (with-output-to-string (sequence)
    (when (member :default-foreground imposed)
      (write-string (default-color-reset-sequence :foreground) sequence))
    (when (member :default-background imposed)
      (write-string (default-color-reset-sequence :background) sequence))
    (format sequence "~c[0m" +escape-character+)
    (loop for (number name default-set-p) in *mode-tracking-private-modes*
          when (member name imposed)
            do (format sequence "~c[?~d~:[l~;h~]"
                       +escape-character+ number default-set-p))))

(defun mode-tracking--note (stream name imposed-p)
  "Record whether mode NAME is held away from its default in STREAM."
  (if imposed-p
      (pushnew name (mode-tracking-output-stream--imposed stream))
      (setf (mode-tracking-output-stream--imposed stream)
            (remove name (mode-tracking-output-stream--imposed stream)))))

(defun mode-tracking--begin (stream state)
  "Enter parser STATE with no collected parameters."
  (setf (mode-tracking-output-stream--state stream) state
        (mode-tracking-output-stream--parameters stream) (make-string-output-stream)
        (mode-tracking-output-stream--parameter-count stream) 0))

(defun mode-tracking--collect (stream character)
  "Collect parameter CHARACTER, abandoning the control once it is too long."
  (if (< (mode-tracking-output-stream--parameter-count stream)
         *mode-tracking-parameter-limit*)
      (progn
        (write-char character (mode-tracking-output-stream--parameters stream))
        (incf (mode-tracking-output-stream--parameter-count stream)))
      (setf (mode-tracking-output-stream--state stream) :ground)))

(defun mode-tracking--parameters (stream)
  "Return the collected parameters of STREAM's control as a string."
  (get-output-stream-string (mode-tracking-output-stream--parameters stream)))

(defun mode-tracking--apply-private (stream parameters set-p)
  "Apply one DEC private mode control setting (SET-P) or resetting PARAMETERS."
  (dolist (field (mode-tracking--split parameters))
    (let ((entry (assoc (parse-integer field :junk-allowed t)
                        *mode-tracking-private-modes*)))
      (when entry
        (destructuring-bind (number name default-set-p) entry
          (declare (ignore number))
          (mode-tracking--note stream name
                               (not (eq set-p default-set-p))))))))

(defun mode-tracking--apply-color (stream parameters terminator)
  "Apply one operating system command numbered PARAMETERS ended by TERMINATOR.

OSC 10 and 11 impose a default color once their payload begins; OSC 110 and
111 restore it whatever ends them."
  (cond
    ((and (char= terminator #\;) (string= parameters "10"))
     (mode-tracking--note stream :default-foreground t))
    ((and (char= terminator #\;) (string= parameters "11"))
     (mode-tracking--note stream :default-background t))
    ((string= parameters "110")
     (mode-tracking--note stream :default-foreground nil))
    ((string= parameters "111")
     (mode-tracking--note stream :default-background nil))))

(defun mode-tracking--split (parameters)
  "Return the semicolon-separated fields of PARAMETERS."
  (loop for start = 0 then (1+ end)
        for end = (position #\; parameters :start start)
        collect (subseq parameters start end)
        while end))

(defun mode-tracking--track (stream character)
  "Advance STREAM's control parser over one output CHARACTER."
  (let ((state (mode-tracking-output-stream--state stream)))
    (cond
      ((and (char= character +escape-character+) (not (eq state :osc)))
       (setf (mode-tracking-output-stream--state stream) :escape))
      (t
       (ecase state
         (:ground
          nil)
         (:escape
          (case character
            (#\[ (mode-tracking--begin stream :csi))
            (#\] (mode-tracking--begin stream :osc))
            (t (setf (mode-tracking-output-stream--state stream) :ground))))
         (:csi
          (setf (mode-tracking-output-stream--state stream)
                (if (char= character #\?) :private :ground)))
         (:private
          (cond
            ((or (digit-char-p character) (char= character #\;))
             (mode-tracking--collect stream character))
            ((member character '(#\h #\l))
             (mode-tracking--apply-private stream
                                           (mode-tracking--parameters stream)
                                           (char= character #\h))
             (setf (mode-tracking-output-stream--state stream) :ground))
            (t
             (setf (mode-tracking-output-stream--state stream) :ground))))
         (:osc
          (if (digit-char-p character)
              (mode-tracking--collect stream character)
              (progn
                (mode-tracking--apply-color stream
                                            (mode-tracking--parameters stream)
                                            character)
                (setf (mode-tracking-output-stream--state stream)
                      (if (char= character +escape-character+) :escape :ground))))))))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream mode-tracking-output-stream) character)
  "Track CHARACTER, then forward it unchanged."
  (mode-tracking--track stream character)
  (write-char character (mode-tracking-output-stream-output stream)))

(defmethod trivial-gray-streams:stream-write-string
    ((stream mode-tracking-output-stream) string &optional (start 0) end)
  "Track every character of STRING, then forward it unchanged."
  (let ((end (or end (length string))))
    (loop for index from start below end
          do (mode-tracking--track stream (char string index)))
    (write-string string (mode-tracking-output-stream-output stream)
                  :start start :end end)))

(defmethod trivial-gray-streams:stream-line-column ((stream mode-tracking-output-stream))
  "Report no column, since forwarded controls make columns unknowable."
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((stream mode-tracking-output-stream))
  "Finish the destination's output."
  (finish-output (mode-tracking-output-stream-output stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream mode-tracking-output-stream))
  "Force the destination's output."
  (force-output (mode-tracking-output-stream-output stream)))
