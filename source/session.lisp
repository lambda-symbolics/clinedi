(in-package #:clinedi)

;;;; -- Modal selection sessions --

(defclass selection-session ()
  ((selector :accessor selection-session-selector
             :documentation "Current filtered navigation state.")
   (items :initarg :items :accessor selection-session-items
          :documentation "Unfiltered opaque candidates.")
   (query :initform "" :accessor selection-session-query
          :documentation "Sanitized search query.")
   (identity-key :initarg :identity-key :reader selection-session-identity-key
                 :documentation "Function returning a stable candidate identity.")
   (identity-test :initarg :identity-test :reader selection-session-identity-test
                  :documentation "Equality predicate for candidate identities.")
   (search-key :initarg :search-key :reader selection-session-search-key
               :documentation "Function returning searchable candidate text.")
   (search-p :initarg :search-p :reader selection-session-search-p
             :documentation "Whether insertion events edit the query.")
   (query-limit :initarg :query-limit :reader selection-session-query-limit
                :documentation "Maximum retained query characters.")
   (visible-count :initarg :visible-count :reader selection-session-visible-count
                  :documentation "Maximum visible candidate rows."))
  (:documentation "Filtered modal navigation with stable candidate identity."))

(defun selection-session-selected-id (session)
  "Return the selected candidate's stable identity, or NIL."
  (let ((item (selector-selected-item (selection-session-selector session))))
    (and item (funcall (selection-session-identity-key session) item))))

(defun selection-session-select-id (session identity)
  "Select IDENTITY when it occurs in SESSION's filtered candidates."
  (let* ((selector (selection-session-selector session))
         (position (position identity (selector-items selector)
                             :key (selection-session-identity-key session)
                             :test (selection-session-identity-test session))))
    (when position
      (selector-move selector (- position (selector-selection selector)))))
  session)

(defun selection-session--terms (query)
  "Split QUERY at whitespace into nonempty search terms."
  (loop with start = 0
        for end = (position-if (lambda (character)
                                 (find character '(#\Space #\Tab #\Newline #\Return)))
                               query :start start)
        when (< start (or end (length query)))
          collect (subseq query start end)
        while end
        do (setf start (1+ end))))

(defun selection-session--refresh (session &optional selected-id)
  "Rebuild filtered navigation, retaining SELECTED-ID when possible."
  (let* ((terms (selection-session--terms (selection-session-query session)))
         (items (selection-session-items session))
         (matches
           (if terms
               (remove-if-not
                (lambda (item)
                  (let ((text (funcall (selection-session-search-key session) item)))
                    (check-type text string)
                    (every (lambda (term) (search term text :test #'char-equal))
                           terms)))
                items)
               items)))
    (setf (selection-session-selector session)
          (make-selector :items matches
                         :visible-count (selection-session-visible-count session)
                         :arrangement ':vertical))
    (when selected-id
      (selection-session-select-id session selected-id)))
  session)

(defun make-selection-session (&key items (identity-key #'identity)
                                    (identity-test #'equal) (search-key #'identity)
                                    search-p (query-limit 256) (visible-count 6)
                                    initial-id)
  "Create a filtered modal session over opaque ITEMS.

IDENTITY-KEY returns a stable designator used across filtering and replacement.
SEARCH-KEY returns text; whitespace-separated query terms match case-insensitively."
  (check-type query-limit (integer 0))
  (let ((session (make-instance 'selection-session :items (copy-list items)
                                :identity-key identity-key :identity-test identity-test
                                :search-key search-key :search-p search-p
                                :query-limit query-limit :visible-count visible-count)))
    (selection-session--refresh session initial-id)))

(defun selection-session-replace-items (session items &key (selected-id nil supplied-p)
                                                          (reset-query-p t))
  "Replace SESSION candidates and optionally reset its query or selected identity."
  (let ((identity (if supplied-p selected-id (selection-session-selected-id session))))
    (setf (selection-session-items session) (copy-list items))
    (when reset-query-p
      (setf (selection-session-query session) ""))
    (selection-session--refresh session identity)))

(defun selection-session-handle-event (session event)
  "Apply EVENT and return selector action and opaque candidate as two values."
  (let ((query (selection-session-query session))
        (identity (selection-session-selected-id session)))
    (cond
      ((and (selection-session-search-p session)
            (or (member event '(:backspace :kill-line))
                (and (consp event) (member (first event) '(:insert :paste))
                     (stringp (second event)))))
       (setf (selection-session-query session)
             (cond
               ((eq event ':kill-line) "")
               ((eq event ':backspace)
                (subseq query 0 (grapheme-previous-boundary query (length query))))
               (t
                (let* ((text (sanitize-text (second event) :single-line-p t))
                       (remaining (max 0 (- (selection-session-query-limit session)
                                             (length query)))))
                  (concatenate 'string query
                               (subseq text 0 (min remaining (length text))))))))
       (selection-session--refresh session identity)
       (values ':continue nil))
      ((eq event ':poll)
       (values ':continue nil))
      (t
       (selector-handle-event (selection-session-selector session) event)))))

(defun run-selection-session (session &key read-event input-ready-p poll-interval
                                          refresh paint on-event on-open on-close
                                          (call-with-lock #'funcall)
                                          (wait #'sleep))
  "Run SESSION with caller-supplied transport, presentation and locking callbacks.

Query REFRESH before readiness and again before event handling. If it returns
true, omit the redundant PAINT. With POLL-INTERVAL, deliver :POLL when input is
not ready. ON-EVENT receives (EVENT SELECTOR), returning NIL for default handling,
:CONTINUE, (:ACCEPT VALUE), or (:CANCEL). Return the accepted opaque value or NIL.
Call ON-CLOSE on every exit, including partial ON-OPEN failure. All callbacks
except READ-EVENT, INPUT-READY-P and WAIT run through CALL-WITH-LOCK."
  (labels ((locked (function) (funcall call-with-lock function))

           (refresh () (and refresh (funcall refresh))))
    (unwind-protect
         (progn
           (when on-open (locked on-open))
           (loop
             (locked (lambda ()
                       (unless (refresh)
                         (when paint (funcall paint)))))
             (let ((event (if (and poll-interval input-ready-p
                                   (not (funcall input-ready-p)))
                              (progn (funcall wait poll-interval) ':poll)
                              (funcall read-event))))
               (when (eq event ':stream-end)
                 (return nil))
               (locked
                (lambda ()
                  (refresh)
                  (let ((custom (and on-event
                                      (funcall on-event event
                                               (selection-session-selector session)))))
                    (cond
                      ((null custom)
                       (multiple-value-bind (action item)
                           (selection-session-handle-event session event)
                         (case action
                           ((:accept :dismiss) (return-from run-selection-session item))
                           (:cancel (return-from run-selection-session nil)))))
                      ((eq custom ':continue) nil)
                      ((and (consp custom) (eq (first custom) ':accept))
                       (return-from run-selection-session (second custom)))
                      ((and (consp custom) (eq (first custom) ':cancel))
                       (return-from run-selection-session nil))
                      (t
                       (error 'type-error :datum custom
                              :expected-type '(or null (eql :continue) cons))))))))))
      (when on-close (locked on-close)))))
