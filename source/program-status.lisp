;;;; -- OSC 7501 program status --

(in-package #:clinedi)

;;; The OSC 7501 Program Status Protocol lets a program report its own state,
;;; and the state of independent records such as background jobs, to the
;;; terminal: https://www.superlogical.com/rex/docs/build/program-status.
;;; Builders return control strings, like the other sequence functions here,
;;; so the caller decides when and through which transport they are written.

(defparameter *program-status-record-limit* 63
  "The most records one program keeps reported to the terminal at once.")

(defparameter *program-status-title-octets* 192
  "The longest title, in UTF-8 octets, a report carries.")

(defparameter *program-status-message-octets* 2048
  "The longest message, in UTF-8 octets, a report carries.")

(defparameter *program-status-states*
  '(:idle :working :done :blocked :error :clear)
  "The states a program status report may name.")

(defparameter *program-status-blocked-kinds* '(:permission :question :auth)
  "What a blocked report may say the program is waiting for.")

(defclass program-status-records ()
  ((application :initarg :application :reader program-status-records-application
                :documentation "The application name every record is reported under.")
   (records :initform nil :accessor program-status-records-current
            :documentation "The records last reported, as plists of :ID, :STATE, :TITLE and :MESSAGE."))
  (:documentation "The independent records one program keeps reported to a terminal.

Updates send only changed records and clear removed ones, and the working
records can be replayed after a semantic prompt start expires them."))

(defun program-status-sequence (application state &key id kind progress message title)
  "Return one OSC 7501 report for APPLICATION in STATE.

ID names an independent record, such as jobs/1234; without it the report is
about the program itself. KIND says what a :BLOCKED program waits for. PROGRESS
is reported only as an integer percentage of :WORKING or :BLOCKED work, and is
otherwise omitted. MESSAGE and TITLE lose their control characters and are
truncated at whole UTF-8 characters to their octet limits."
  (unless (program-status--token-p application)
    (error 'program-status-error :field ':application :value application :state state))
  (unless (member state *program-status-states*)
    (error 'program-status-error :field ':state :value state :state state))
  (unless (or (null id) (program-status-identifier-p id))
    (error 'program-status-error :field ':id :value id :state state))
  (unless (if (eq state :blocked)
              (member kind *program-status-blocked-kinds*)
              (null kind))
    (error 'program-status-error :field ':kind :value kind :state state))
  (ansi--operating-system-command
   (format nil "7501;app=~a:state=~(~a~)~@[:id=~a~]~@[:kind=~(~a~)~]~@[:progress=~d~]~@[:msg=~a~]~@[:title=~a~]"
           application state id kind
           (and (member state '(:working :blocked))
                (typep progress '(integer 0 100))
                progress)
           (and message (program-status-text message *program-status-message-octets*))
           (and title (program-status-text title *program-status-title-octets*)))))

(defun program-status-identifier-p (id)
  "Return true when ID is a record path: up to eight slash-separated short tokens."
  (and (stringp id)
       (<= 1 (length id) 128)
       (let ((segments (program-status--split id #\/)))
         (and (<= (length segments) 8)
              (every #'program-status--token-p segments)))
       t))

(defun program-status-text (text limit)
  "Return TEXT without control characters as base64 UTF-8 of at most LIMIT octets.

Truncation keeps whole characters, so the decoded text is always valid UTF-8."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for character across text
          for code = (char-code character)
          for encoded = (program-status--character-octets code)
          unless (or (< code 32) (<= 127 code 159) (<= #xd800 code #xdfff))
            do (when (> (+ (length octets) (length encoded)) limit)
                 (return))
               (dolist (octet encoded)
                 (vector-push-extend octet octets)))
    (program-status--base64 octets)))

(defun program-status-records-update (records new)
  "Report NEW records, plists of :ID, :STATE, :TITLE and :MESSAGE, in place of the previous ones.

Only the newest *PROGRAM-STATUS-RECORD-LIMIT* records are kept. Return the
controls that clear removed records and send changed ones, which is empty when
nothing changed."
  (let ((previous (program-status-records-current records))
        (current  (last new *program-status-record-limit*))
        (application (program-status-records-application records)))
    (setf (program-status-records-current records) current)
    (with-output-to-string (stream)
      (dolist (record previous)
        (unless (program-status--find (getf record :id) current)
          (write-string (program-status-sequence application :clear :id (getf record :id))
                        stream)))
      (dolist (record current)
        (unless (equal record (program-status--find (getf record :id) previous))
          (write-string (program-status--record-sequence application record) stream))))))

(defun program-status-records-replay (records)
  "Return the controls resending RECORDS that are still working.

A semantic prompt start expires a terminal's working records, so a program
replays them right after writing one."
  (with-output-to-string (stream)
    (dolist (record (program-status-records-current records))
      (when (eq (getf record :state) :working)
        (write-string (program-status--record-sequence
                       (program-status-records-application records) record)
                      stream)))))

(defun program-status-records-reset (records)
  "Forget RECORDS without clearing them, as when the terminal cannot show them."
  (setf (program-status-records-current records) nil)
  records)

(defun program-status--record-sequence (application record)
  "Return the report for one RECORD plist under APPLICATION."
  (program-status-sequence application (getf record :state)
                           :id      (getf record :id)
                           :message (getf record :message)
                           :title   (getf record :title)))

(defun program-status--find (id records)
  "Return the record named ID among RECORDS, or NIL."
  (find id records :key (lambda (record) (getf record :id)) :test #'string=))

(defun program-status--token-p (token)
  "Return true when TOKEN is one to 32 letters, digits, or _.+- characters."
  (and (stringp token)
       (<= 1 (length token) 32)
       (every (lambda (character)
                (or (char<= #\a character #\z) (char<= #\A character #\Z)
                    (char<= #\0 character #\9) (find character "_.+-")))
              token)
       t))

(defun program-status--split (string separator)
  "Split STRING at each SEPARATOR, keeping empty fields."
  (loop for start = 0 then (1+ end)
        for end = (position separator string :start start)
        collect (subseq string start end)
        while end))

(defun program-status--character-octets (code)
  "Return the UTF-8 octets encoding code point CODE."
  (cond ((< code #x80)
         (list code))
        ((< code #x800)
         (list (logior #xc0 (ash code -6))
               (logior #x80 (logand code #x3f))))
        ((< code #x10000)
         (list (logior #xe0 (ash code -12))
               (logior #x80 (logand (ash code -6) #x3f))
               (logior #x80 (logand code #x3f))))
        (t
         (list (logior #xf0 (ash code -18))
               (logior #x80 (logand (ash code -12) #x3f))
               (logior #x80 (logand (ash code -6) #x3f))
               (logior #x80 (logand code #x3f))))))

(defun program-status--base64 (octets)
  "Return OCTETS as padded standard base64."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (stream)
      (loop for index from 0 below (length octets) by 3
            for remaining = (- (length octets) index)
            for group = (logior (ash (aref octets index) 16)
                                (if (> remaining 1) (ash (aref octets (+ index 1)) 8) 0)
                                (if (> remaining 2) (aref octets (+ index 2)) 0))
            do (dotimes (position 4)
                 (write-char (if (< position (1+ (min remaining 3)))
                                 (char alphabet (logand (ash group (* -6 (- 3 position))) #x3f))
                                 #\=)
                             stream))))))


;;;; -- Conditions --

(define-condition program-status-error (error)
  ((field
    :initarg       :field
    :reader        program-status-error-field
    :documentation "The rejected report field: :APPLICATION, :STATE, :ID or :KIND.")
   (value
    :initarg       :value
    :reader        program-status-error-value
    :documentation "The rejected value of FIELD.")
   (state
    :initarg       :state
    :reader        program-status-error-state
    :documentation "The program state the report was for."))
  (:report
   (lambda (condition stream)
     (let ((value (program-status-error-value condition))
           (state (program-status-error-state condition)))
       (ecase (program-status-error-field condition)
         (:application
          (format stream "Program status application ~S is not a short token." value))
         (:state
          (format stream "Program status state ~S is not one of ~S." value *program-status-states*))
         (:id
          (format stream "Program status record identifier ~S is not a valid path." value))
         (:kind
          (format stream "Program status kind ~S does not fit state ~S." value state))))))
  (:documentation "A program status report field has a value the protocol does not allow."))
