(in-package #:clinedi)

;;;; -- Input pump --

;;; An input pump owns the one thread that reads a terminal for a
;;; multi-threaded program. It polls a host READY predicate and runs a host STEP
;;; that reads and handles one event. Modal work such as a picker takes the
;;; terminal for itself with INPUT-PUMP-CALL-WITH-EXCLUSIVE-INPUT: on the
;;; reader thread it simply runs, elsewhere it pauses the reader, which is
;;; joined, and restarts it afterwards. Pauses nest, so only the outermost one
;;; stops and restarts the thread.

(defclass input-pump ()
  ((name :initarg :name :reader input-pump-name
         :documentation "The reader thread's name.")
   (ready-function :initarg :ready-function :reader input-pump-ready-function
                   :documentation "Return true when STEP-FUNCTION can read without blocking.")
   (step-function :initarg :step-function :reader input-pump-step-function
                  :documentation "Read and handle one event; returning :STOP ends the reader.")
   (tick-function :initarg :tick-function :reader input-pump-tick-function
                  :documentation "Called at the start of every reader iteration, or NIL.")
   (startable-p-function :initarg :startable-p-function
                         :reader input-pump-startable-p-function
                         :documentation "Return true when the reader may start or restart.")
   (failure-function :initarg :failure-function :reader input-pump-failure-function
                     :documentation "Receive a serious condition that ended the reader and its backtrace.")
   (backtrace-function :initarg :backtrace-function
                       :reader input-pump-backtrace-function
                       :documentation "Return a backtrace string where a condition is signaled, or NIL.")
   (poll-seconds :initarg :poll-seconds :reader input-pump-poll-seconds
                 :documentation "The longest idle wait before polling input again.")
   (lock :initform (bt2:make-lock :name "clinedi input pump") :reader input-pump--lock
         :documentation "The lock guarding the reader state.")
   (condition-variable :initform (bt2:make-condition-variable :name "clinedi input pump")
                       :reader input-pump--condition-variable
                       :documentation "Wakes an idle or pausing reader.")
   (thread :initform nil :accessor input-pump--thread
           :documentation "The live reader thread, or NIL.")
   (paused-p :initform nil :accessor input-pump--paused-p
             :documentation "Whether the reader must stay stopped.")
   (pause-depth :initform 0 :accessor input-pump--pause-depth
                :documentation "Nested pauses keeping the reader stopped.")
   (stopped-p :initform nil :accessor input-pump--stopped-p
              :documentation "Whether the pump was stopped for good."))
  (:documentation "The single, pausable terminal reader thread of a program."))

(define-condition input-pump-error (error)
  ((message :initarg :message :reader input-pump-error-message
            :documentation "A concise description of the misuse."))
  (:documentation "An input pump was used in a way it cannot honor.")
  (:report (lambda (condition stream)
             (write-string (input-pump-error-message condition) stream))))

(defun make-input-pump (&key ready-function step-function tick-function
                             (startable-p-function (constantly t))
                             (failure-function (constantly nil))
                             (backtrace-function (constantly nil))
                             (poll-seconds 0.02)
                             (name "clinedi input"))
  "Create a stopped input pump.

READY-FUNCTION returns true when input can be read without blocking, and
STEP-FUNCTION reads and handles one event, returning :STOP to end the reader.
TICK-FUNCTION, when given, runs at the start of every iteration.
STARTABLE-P-FUNCTION is consulted before every start or restart. A serious
condition escaping the reader ends it and is passed with the string
BACKTRACE-FUNCTION returned where it was signaled to FAILURE-FUNCTION, on the
reader thread."
  (check-type ready-function function)
  (check-type step-function function)
  (make-instance 'input-pump
                 :name name
                 :ready-function ready-function
                 :step-function step-function
                 :tick-function tick-function
                 :startable-p-function startable-p-function
                 :failure-function failure-function
                 :backtrace-function backtrace-function
                 :poll-seconds poll-seconds))

(defun input-pump-start (pump)
  "Start PUMP's reader unless it is paused, stopped, already live or not startable."
  (when (funcall (input-pump-startable-p-function pump))
    (bt2:with-lock-held ((input-pump--lock pump))
      (unless (or (input-pump--stopped-p pump)
                  (input-pump--paused-p pump)
                  (let ((thread (input-pump--thread pump)))
                    (and thread (bt2:thread-alive-p thread))))
        (setf (input-pump--thread pump)
              (bt2:make-thread (lambda () (input-pump--run pump))
                               :name (input-pump-name pump))))))
  pump)

(defun input-pump-wake (pump)
  "End an idle wait of PUMP's reader early, so it polls input again now."
  (bt2:with-lock-held ((input-pump--lock pump))
    (bt2:condition-broadcast (input-pump--condition-variable pump)))
  pump)

(defun input-pump-stop (pump)
  "Stop PUMP's reader for good and wait for it to finish."
  (bt2:with-lock-held ((input-pump--lock pump))
    (setf (input-pump--stopped-p pump) t
          (input-pump--paused-p pump) t))
  (input-pump--join pump)
  pump)

(defun input-pump-live-p (pump)
  "Return true while PUMP's reader thread is running."
  (bt2:with-lock-held ((input-pump--lock pump))
    (let ((thread (input-pump--thread pump)))
      (and thread (bt2:thread-alive-p thread) t))))

(defun input-pump-paused-p (pump)
  "Return true while PUMP's reader is held stopped by a pause or by stopping."
  (bt2:with-lock-held ((input-pump--lock pump))
    (and (input-pump--paused-p pump) t)))

(defun input-pump-reader-thread-p (pump)
  "Return true when the current thread is PUMP's reader."
  (bt2:with-lock-held ((input-pump--lock pump))
    (eq (bt2:current-thread) (input-pump--thread pump))))

(defun input-pump-call-with-input-paused (pump function)
  "Call FUNCTION while PUMP's reader is stopped, restarting it afterwards.

Pauses nest; the outermost one joins the reader and, when it ends, restarts it
if the pump was not stopped and is still startable. Calling this on the reader
thread signals INPUT-PUMP-ERROR, since a reader cannot wait for itself."
  (let ((outermost-p nil))
    (bt2:with-lock-held ((input-pump--lock pump))
      (when (eq (bt2:current-thread) (input-pump--thread pump))
        (error 'input-pump-error
               :message "Work running on the input reader cannot pause it."))
      (setf outermost-p (zerop (input-pump--pause-depth pump)))
      (incf (input-pump--pause-depth pump))
      (when outermost-p
        (setf (input-pump--paused-p pump) t)))
    (when outermost-p
      (input-pump--join pump))
    (unwind-protect
         (funcall function)
      (let ((restart-p nil))
        (bt2:with-lock-held ((input-pump--lock pump))
          (when (zerop (decf (input-pump--pause-depth pump)))
            (unless (input-pump--stopped-p pump)
              (setf (input-pump--paused-p pump) nil
                    restart-p t))))
        (when restart-p
          (input-pump-start pump))))))

(defun input-pump-call-with-exclusive-input (pump function)
  "Call FUNCTION as the only reader of PUMP's terminal.

On the reader thread FUNCTION already is that reader and runs in place; any
other thread pauses the reader for FUNCTION's dynamic extent."
  (if (input-pump-reader-thread-p pump)
      (funcall function)
      (input-pump-call-with-input-paused pump function)))

(defun input-pump--join (pump)
  "Wake PUMP's reader so it sees its pause, then wait for it to finish."
  (let ((thread nil))
    (bt2:with-lock-held ((input-pump--lock pump))
      (setf thread (input-pump--thread pump))
      (bt2:condition-broadcast (input-pump--condition-variable pump)))
    (when (and thread (not (eq thread (bt2:current-thread))))
      (bt2:join-thread thread)
      (bt2:with-lock-held ((input-pump--lock pump))
        (when (eq thread (input-pump--thread pump))
          (setf (input-pump--thread pump) nil)))))
  nil)

(defun input-pump--paused-now-p (pump)
  "Return true when PUMP's reader must stop now."
  (bt2:with-lock-held ((input-pump--lock pump))
    (input-pump--paused-p pump)))

(defun input-pump--idle (pump)
  "Wait up to the poll interval unless PUMP is paused or woken."
  (bt2:with-lock-held ((input-pump--lock pump))
    (unless (input-pump--paused-p pump)
      (bt2:condition-wait (input-pump--condition-variable pump)
                          (input-pump--lock pump)
                          :timeout (input-pump-poll-seconds pump))))
  nil)

(defun input-pump--run (pump)
  "Run PUMP's reader loop until a pause, a :STOP step, or a failure."
  (let ((backtrace nil))
    ;; The backtrace handler must be inside HANDLER-CASE: handlers run innermost
    ;; first, and HANDLER-CASE unwinds before any outer handler could look.
    (handler-case
        (handler-bind ((serious-condition
                         (lambda (condition)
                           (declare (ignore condition))
                           (setf backtrace
                                 (ignore-errors
                                  (funcall (input-pump-backtrace-function pump)))))))
          (loop
            (let ((tick (input-pump-tick-function pump)))
              (when tick
                (funcall tick)))
            (when (input-pump--paused-now-p pump)
              (return))
            (if (funcall (input-pump-ready-function pump))
                (when (eq (funcall (input-pump-step-function pump)) :stop)
                  (return))
                (input-pump--idle pump))))
      (serious-condition (condition)
        (funcall (input-pump-failure-function pump) condition backtrace))))
  nil)
