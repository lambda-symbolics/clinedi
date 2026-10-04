(in-package #:clinedi/tests)

;;;; -- Input pump --

(defclass input-pump-tests--source ()
  ((lock :initform (bt2:make-lock :name "input pump test source") :reader source-lock)
   (pending :initform nil :accessor source-pending)
   (handled :initform nil :accessor source-handled))
  (:documentation "A queue of fake terminal events and the events a pump handled."))

(defun input-pump-tests--push (source &rest events)
  "Make EVENTS readable from SOURCE in order."
  (bt2:with-lock-held ((source-lock source))
    (setf (source-pending source) (append (source-pending source) events))))

(defun input-pump-tests--handled (source)
  "Return the events SOURCE's pump handled, oldest first."
  (bt2:with-lock-held ((source-lock source))
    (reverse (source-handled source))))

(defun input-pump-tests--wait (predicate)
  "Return true once PREDICATE holds, polling for at most five seconds."
  (loop repeat 500
        thereis (funcall predicate)
        do (sleep 0.01)))

(defun input-pump-tests--make (source &rest arguments &key step-function &allow-other-keys)
  "Create a pump reading SOURCE, handling events with STEP-FUNCTION when given."
  (apply #'clinedi:make-input-pump
         :ready-function (lambda ()
                           (bt2:with-lock-held ((source-lock source))
                             (and (source-pending source) t)))
         :step-function
         (or step-function
             (lambda ()
               (bt2:with-lock-held ((source-lock source))
                 (let ((event (pop (source-pending source))))
                   (push event (source-handled source))
                   (and (eq event :quit) :stop)))))
         :poll-seconds 0.01
         (loop for (key value) on arguments by #'cddr
               unless (eq key :step-function)
                 append (list key value))))

(defun run-input-pump-tests ()
  "Test reading, nested pauses, exclusive input, stopping and reader failures."
  (let* ((source (make-instance 'input-pump-tests--source))
         (pump (input-pump-tests--make source)))
    (unwind-protect
         (progn
           (clinedi:input-pump-start pump)
           (input-pump-tests--push source :a :b)
           (check-true "a started pump handles events in order"
                       (input-pump-tests--wait
                        (lambda () (equal (input-pump-tests--handled source) '(:a :b)))))
           (let ((seen-inside nil))
             (clinedi:input-pump-call-with-input-paused
              pump
              (lambda ()
                (clinedi:input-pump-call-with-input-paused
                 pump
                 (lambda () (input-pump-tests--push source :c)))
                (sleep 0.05)
                (setf seen-inside (list (clinedi:input-pump-live-p pump)
                                        (clinedi:input-pump-paused-p pump)
                                        (input-pump-tests--handled source)))))
             (check-equal "a nested pause keeps the reader stopped until the outermost ends"
                          '(nil t (:a :b)) seen-inside))
           (check-true "the reader restarts after the outermost pause and reads what arrived"
                       (input-pump-tests--wait
                        (lambda () (equal (input-pump-tests--handled source) '(:a :b :c)))))
           (let ((thread (bt2:current-thread)))
             (check-equal "exclusive input from another thread pauses the reader"
                          '(nil nil)
                          (clinedi:input-pump-call-with-exclusive-input
                           pump
                           (lambda ()
                             (list (clinedi:input-pump-live-p pump)
                                   (not (eq thread (bt2:current-thread))))))))
           (input-pump-tests--push source :quit)
           (check-true "a step returning :STOP ends the reader"
                       (input-pump-tests--wait (lambda () (not (clinedi:input-pump-live-p pump)))))
           (clinedi:input-pump-start pump)
           (check-true "a pump ended by its step starts again"
                       (clinedi:input-pump-live-p pump))
           (clinedi:input-pump-stop pump)
           (clinedi:input-pump-start pump)
           (clinedi:input-pump-call-with-input-paused pump (lambda () nil))
           (check-equal "a stopped pump neither starts nor restarts after a pause"
                        '(nil t)
                        (list (clinedi:input-pump-live-p pump)
                              (clinedi:input-pump-paused-p pump))))
      (clinedi:input-pump-stop pump)))
  (let* ((source (make-instance 'input-pump-tests--source))
         (outcomes nil)
         (pump nil))
    (setf pump
          (input-pump-tests--make
           source
           :step-function
           (lambda ()
             (bt2:with-lock-held ((source-lock source))
               (pop (source-pending source)))
             (push (list (clinedi:input-pump-call-with-exclusive-input
                          pump (lambda () (clinedi:input-pump-reader-thread-p pump)))
                         (handler-case
                             (clinedi:input-pump-call-with-input-paused pump (lambda () :paused))
                           (clinedi:input-pump-error () :refused)))
                   outcomes)
             :stop)))
    (unwind-protect
         (progn
           (clinedi:input-pump-start pump)
           (input-pump-tests--push source :modal)
           (check-true "work on the reader thread finishes"
                       (input-pump-tests--wait
                        (lambda () (not (clinedi:input-pump-live-p pump)))))
           (check-equal "exclusive input runs in place on the reader, which cannot pause itself"
                        '((t :refused)) outcomes))
      (clinedi:input-pump-stop pump)))
  (let* ((source (make-instance 'input-pump-tests--source))
         (failure nil)
         (pump (input-pump-tests--make
                source
                :step-function (lambda () (error "reader broke"))
                :backtrace-function (lambda () "frames")
                :failure-function (lambda (condition backtrace)
                                    (setf failure (list (princ-to-string condition)
                                                        backtrace))))))
    (unwind-protect
         (progn
           (clinedi:input-pump-start pump)
           (input-pump-tests--push source :boom)
           (check-true "a failing step reports its condition and backtrace"
                       (input-pump-tests--wait
                        (lambda () (equal failure '("reader broke" "frames"))))))
      (clinedi:input-pump-stop pump)))
  (let* ((source (make-instance 'input-pump-tests--source))
         (pump (input-pump-tests--make source :startable-p-function (constantly nil))))
    (clinedi:input-pump-start pump)
    (check-equal "an unstartable pump does not start" nil (clinedi:input-pump-live-p pump))))
