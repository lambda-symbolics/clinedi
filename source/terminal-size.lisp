(in-package #:clinedi)

;;;; -- Terminal Geometry --

(defun terminal-current-size
    (&key (file-descriptor
            (when (fboundp 'terminal-standard-input-file-descriptor)
              (funcall 'terminal-standard-input-file-descriptor)))
          (terminal-io *terminal-io*)
          (default-rows *terminal-default-rows*)
          (default-columns *terminal-default-columns*))
  "Return positive terminal rows and columns as two values.

For each dimension, prefer the native adapter's FILE-DESCRIPTOR size, then
interactive TERMINAL-IO's tput result, LINES or COLUMNS, and the caller's default.
Without a loaded native adapter, or with NIL FILE-DESCRIPTOR, skip native lookup."
  (check-type file-descriptor (or null integer))
  (check-type terminal-io stream)
  (check-type default-rows (integer 1))
  (check-type default-columns (integer 1))
  (multiple-value-bind (rows columns)
      (if (and file-descriptor (fboundp 'terminal-file-descriptor-size))
          (funcall 'terminal-file-descriptor-size file-descriptor)
          (values nil nil))
    (values
     (or rows
         (terminal--query-dimension "lines" terminal-io)
         (terminal--positive-integer-or-nil (uiop:getenv "LINES"))
         default-rows)
     (or columns
         (terminal--query-dimension "cols" terminal-io)
         (terminal--positive-integer-or-nil (uiop:getenv "COLUMNS"))
         default-columns))))

(defun terminal--query-dimension (capability terminal-io)
  "Return positive tput CAPABILITY output when TERMINAL-IO is interactive."
  (when (interactive-stream-p terminal-io)
    (handler-case
        (terminal--positive-integer-or-nil
         (uiop:run-program (list "tput" capability)
                           :output ':string
                           :error-output ':output))
      (error ()
        nil))))

(defun terminal--positive-integer-or-nil (value)
  "Parse VALUE as a positive integer, returning NIL on failure."
  (handler-case
      (let ((parsed (and value (plusp (length value))
                         (parse-integer value :junk-allowed t))))
        (and parsed (plusp parsed) parsed))
    (error ()
      nil)))
