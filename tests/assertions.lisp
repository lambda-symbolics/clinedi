;;;; -- Test assertions --

(in-package #:clinedi/tests)

(defvar *test-failures* nil
  "Descriptions of failures from the current test run.")

(defun check-equal (name expected actual)
  "Record a failure under NAME unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (push (format nil "~a: expected ~s, got ~s" name expected actual)
          *test-failures*))
  (values))

(defun check-true (name value)
  "Record a failure under NAME unless VALUE is true."
  (unless value
    (push (format nil "~a: expected a true value" name) *test-failures*))
  (values))
