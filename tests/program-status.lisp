;;;; -- OSC 7501 program status tests --

(in-package #:clinedi/tests)

(defun program-status-tests--decode (text)
  "Return the code points of base64 UTF-8 TEXT, decoded for checking."
  (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (bits 0) (count 0) (octets nil))
    (loop for character across text
          for value = (position character alphabet)
          when value
            do (setf bits (logior (ash bits 6) value)
                     count (+ count 6))
               (when (>= count 8)
                 (decf count 8)
                 (push (logand (ash bits (- count)) #xff) octets)
                 (setf bits (logand bits (1- (ash 1 count))))))
    (setf octets (nreverse octets))
    (values (loop while octets
                  collect (let ((lead (pop octets)))
                            (cond ((< lead #x80)
                                   lead)
                                  ((< lead #xe0)
                                   (logior (ash (logand lead #x1f) 6)
                                           (logand (pop octets) #x3f)))
                                  ((< lead #xf0)
                                   (logior (ash (logand lead #x0f) 12)
                                           (ash (logand (pop octets) #x3f) 6)
                                           (logand (pop octets) #x3f)))
                                  (t
                                   (logior (ash (logand lead #x07) 18)
                                           (ash (logand (pop octets) #x3f) 12)
                                           (ash (logand (pop octets) #x3f) 6)
                                           (logand (pop octets) #x3f))))))
            (floor (* 3 (length (remove #\= text))) 4))))

(defun run-program-status-tests ()
  "Run OSC 7501 framing, text bounds and record set regression checks."
  (dolist (case '((:idle nil) (:working nil) (:done nil) (:error nil)
                  (:clear nil) (:blocked :permission) (:blocked :question) (:blocked :auth)))
    (destructuring-bind (state kind) case
      (check-equal (format nil "~(~a~) report framing" state)
                   (format nil "~c]7501;app=demo:state=~(~a~)~a~c\\"
                           #\Esc state (if kind (format nil ":kind=~(~a~)" kind) "") #\Esc)
                   (program-status-sequence "demo" state :kind kind))))
  (dolist (id '("jobs/0123456789abcdef0123456789abcdef" "a_b.c+d-e" "a/b/c/d/e/f/g/h"))
    (check-equal (format nil "~s is a record path" id) t (program-status-identifier-p id)))
  (dolist (id (list "" "/a" "a/" "a//b" "a:b" "a=b" (make-string 33 :initial-element #\a)
                    "a/b/c/d/e/f/g/h/i" 7))
    (check-equal (format nil "~s is not a record path" id) nil (program-status-identifier-p id))
    (check-equal (format nil "~s is refused as a record" id) t
                 (handler-case (progn (program-status-sequence "demo" :working :id id) nil)
                   (error () t))))
  (check-equal "a blocked report needs its kind" t
               (handler-case (progn (program-status-sequence "demo" :blocked) nil)
                 (error () t)))
  (check-equal "an application name is a token" t
               (handler-case (progn (program-status-sequence "a:b" :idle) nil)
                 (error () t)))
  (let ((text (concatenate 'string "é" (string #\Tab) (string (code-char 159))
                           (make-string 2048 :initial-element #\界))))
    (multiple-value-bind (codes octets)
        (program-status-tests--decode (program-status-text text 2048))
      (check-equal "messages drop controls and keep whole characters"
                   (cons (char-code #\é) (make-list 682 :initial-element (char-code #\界)))
                   codes)
      (check-equal "messages fill their octet limit" 2048 octets))
    (check-equal "titles stay within their octet limit" t
                 (<= (nth-value 1 (program-status-tests--decode (program-status-text text 192)))
                     192))
    (check-equal "a full report stays within 4096 characters" t
                 (<= (length (program-status-sequence "demo" :working :progress 0
                                                      :message text :title text))
                     4096)))
  (check-equal "four-octet characters encode completely"
               (list #x1f600)
               (program-status-tests--decode (program-status-text (string (code-char #x1f600)) 192)))
  (check-equal "progress reports an integer percentage" t
               (not (null (search ":progress=100"
                                  (program-status-sequence "demo" :blocked :kind :auth
                                                                         :progress 100)))))
  (dolist (progress (list nil -1 101 1/2 50.0 "50" :unknown))
    (check-equal (format nil "progress ~s is omitted" progress) nil
                 (search ":progress=" (program-status-sequence "demo" :working
                                                               :progress progress))))
  (check-equal "finished work carries no progress" nil
               (search ":progress=" (program-status-sequence "demo" :done :progress 50)))
  (let* ((records (make-instance 'program-status-records :application "demo"))
         (working (list :id "jobs/1" :state :working :title "Job"))
         (done    (list :id "jobs/1" :state :done :title "Job")))
    (check-equal "new records are sent"
                 (program-status-sequence "demo" :working :id "jobs/1" :title "Job")
                 (program-status-records-update records (list working)))
    (check-equal "unchanged records are not sent twice" ""
                 (program-status-records-update records (list working)))
    (check-equal "working records replay"
                 (program-status-sequence "demo" :working :id "jobs/1" :title "Job")
                 (program-status-records-replay records))
    (check-equal "a changed record updates in place"
                 (program-status-sequence "demo" :done :id "jobs/1" :title "Job")
                 (program-status-records-update records (list done)))
    (check-equal "finished records do not replay" "" (program-status-records-replay records))
    (check-equal "removed records are cleared"
                 (program-status-sequence "demo" :clear :id "jobs/1")
                 (program-status-records-update records nil))
    (program-status-records-update records (list working))
    (program-status-records-reset records)
    (check-equal "a reset forgets records without clearing them" nil
                 (program-status-records-current records))
    (program-status-records-update records (list working))
    (let ((output (program-status-records-update
                   records (loop for index from 2 to 65
                                 collect (list :id (format nil "jobs/~d" index) :state :working)))))
      (check-equal "the record set keeps the newest records" *program-status-record-limit*
                   (length (program-status-records-current records)))
      (check-equal "evicted records are cleared" t
                   (not (null (search (program-status-sequence "demo" :clear :id "jobs/1")
                                      output)))))))
