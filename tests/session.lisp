(in-package #:clinedi/tests)

(defun session-tests--make (&key (search-p t) (query-limit 256))
  "Create duplicate-label candidates with distinct stable identities."
  (clinedi:make-selection-session
   :items '((1 . "same red") (2 . "same blue") (3 . "other red"))
   :identity-key #'car :search-key #'cdr :search-p search-p
   :initial-id 2 :query-limit query-limit :visible-count 2))

(defun run-session-tests ()
  "Test filtered navigation, identity, event coordination and cleanup failures."
  (let ((session (session-tests--make)))
    (dolist (identity '(2 3 1 1 3 2))
      (clinedi:selection-session-select-id session identity)
      (check-equal "identity selection is independent of the current cursor"
                   identity (clinedi:selection-session-selected-id session)))
    (clinedi:selection-session-select-id session 99)
    (check-equal "unknown identity preserves selection" 2
                 (clinedi:selection-session-selected-id session)))
  (let ((session (session-tests--make)))
    (clinedi:selection-session-handle-event session '(:insert "SAME"))
    (check-equal "filter retains identity among duplicate labels" 2
                 (clinedi:selection-session-selected-id session))
    (clinedi:selection-session-handle-event session '(:paste " blue"))
    (check-equal "all case-insensitive terms must match" '((2 . "same blue"))
                 (selector-items (clinedi:selection-session-selector session)))
    (clinedi:selection-session-handle-event session '(:insert " missing"))
    (check-equal "empty match set" nil
                 (selector-selected-item (clinedi:selection-session-selector session)))
    (clinedi:selection-session-handle-event session ':kill-line)
    (check-equal "clear query restores source" 3
                 (length (selector-items (clinedi:selection-session-selector session))))
    (clinedi:selection-session-select-id session 2)
    (clinedi:selection-session-replace-items session '((3 . "new") (2 . "renamed")))
    (check-equal "replacement retains stable identity" 2
                 (clinedi:selection-session-selected-id session))
    (clinedi:selection-session-replace-items session '((3 . "new") (2 . "renamed"))
                                            :selected-id 3)
    (check-equal "replacement can explicitly select" 3
                 (clinedi:selection-session-selected-id session)))
  (let ((session (session-tests--make :query-limit 4)))
    (clinedi:selection-session-handle-event session
      (list ':paste (format nil "e~Cxyz~C[31m" (code-char #x301) #\Escape)))
    (check-equal "bounded sanitized query" 4 (length (clinedi:selection-session-query session)))
    (clinedi:selection-session-handle-event session ':kill-line)
    (clinedi:selection-session-handle-event session (list ':insert (format nil "e~C" (code-char #x301))))
    (clinedi:selection-session-handle-event session ':backspace)
    (check-equal "query deletion removes a complete grapheme" ""
                 (clinedi:selection-session-query session)))
  (dolist (case '(((:submit) 2) ((:down :submit) 3)
                  ((:complete :submit) 3) ((:complete-previous :submit) 1)
                  ((:escape) nil) ((:interrupt) nil) ((:stream-end) nil)))
    (let* ((events (copy-list (first case)))
           (session (session-tests--make))
           (closed 0)
           (result (clinedi:run-selection-session
                    session :read-event (lambda () (pop events))
                    :on-close (lambda () (incf closed)))))
      (check-equal "modal event result" (second case) (car result))
      (check-equal "modal cleanup exactly once" 1 closed)))
  (let ((events nil) (refreshes 0) (paints 0) (polls 0) (locked-p nil))
    (declare (special locked-p))
    (check-equal
     "poll callback acceptance" :chosen
     (clinedi:run-selection-session
      (session-tests--make) :poll-interval 0 :input-ready-p (lambda () nil)
      :read-event (lambda () (error "Unexpected blocking read")) :wait (lambda (seconds) (declare (ignore seconds)))
      :call-with-lock (lambda (function) (let ((locked-p t)) (declare (special locked-p)) (funcall function)))
      :refresh (lambda () (incf refreshes) t)
      :paint (lambda () (incf paints))
      :on-event (lambda (event selector)
                  (declare (ignore selector) (special locked-p))
                  (check-true "event callbacks run locked" locked-p)
                  (push event events) (incf polls)
                  (when (= polls 2) '(:accept :chosen)))))

    (check-equal "poll delivery" '(:poll :poll) events)
    (check-equal "resize before readiness and event" 4 refreshes)
    (check-equal "resize repaint avoids duplicate paint" 0 paints))
  (dolist (phase '(:open :read :refresh :paint :event))
    (let ((closed 0) (failure nil))
      (flet ((fail-at (point) (when (eq point phase) (error "Injected modal failure"))))
        (handler-case
            (clinedi:run-selection-session
             (session-tests--make) :on-open (lambda () (fail-at ':open))
             :read-event (lambda () (fail-at ':read) ':submit)
             :refresh (lambda () (fail-at ':refresh) nil)
             :paint (lambda () (fail-at ':paint))
             :on-event (lambda (event selector) (declare (ignore event selector)) (fail-at ':event))
             :on-close (lambda () (incf closed)))
          (error () (setf failure t))))
      (check-true "injected modal failure propagates" failure)
      (check-equal "cleanup after partial lifecycle" 1 closed)))
  t)
