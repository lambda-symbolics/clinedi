;;;; -- Test runner --

(in-package #:clinedi/tests)

(defun run-tests ()
  "Run all Clinedi tests, signaling an error when any check fails."
  (let ((*test-failures* nil))
    (run-unicode-tests)
    (run-editor-tests)
    (run-editor-state-tests)
    (run-session-tests)
    (run-selector-tests)
    (run-input-tests)
    (run-render-tests)
    (run-live-region-tests)
    (run-terminal-editor-tests)
    (when *test-failures*
      (error "~d Clinedi regression check~:p failed:~%  ~{~a~%  ~}"
             (length *test-failures*)
             (nreverse *test-failures*)))
    (format t "All Clinedi regression checks passed.~%")
    t))
