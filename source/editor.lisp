(in-package #:clinedi)

;;;; -- Incremental Line Editor --

(defconstant +default-history-limit+ 10000
  "Default number of entries retained by a line editor.")

(defparameter *default-word-delimiters*
  '(#\- #\_ #\/ #\. #\:)
  "Characters that separate words in word-delimiter mode by default.")

(defun line-editor--word-delimiters-p (value)
  "Return true when VALUE is a proper list of characters."
  (and (listp value)
       (every #'characterp value)))

(defclass line-editor ()
  ((text
    :initarg :text
    :reader line-editor-text
    :type string
    :documentation "Editable text owned by this editor.")
   (cursor
    :initarg :cursor
    :reader line-editor-cursor
    :type integer
    :documentation "Grapheme boundary in TEXT at which editing occurs.")
   (keymap
    :initarg :keymap
    :initform (default-line-editor-keymap)
    :reader line-editor-keymap
    :type keymap
    :documentation "Programmable event-to-command mappings for this editor.")
   (word-delimiter-mode-p
    :initarg :word-delimiter-mode-p
    :initform nil
    :reader line-editor-word-delimiter-mode-p
    :type boolean
    :documentation
    "Whether configured delimiters separate words for movement and deletion.")
   (word-delimiters
    :initarg :word-delimiters
    :initform (copy-list *default-word-delimiters*)
    :reader line-editor-word-delimiters
    :type (satisfies line-editor--word-delimiters-p)
    :documentation
    "Characters treated as word separators in word-delimiter mode.")
   (history
    :initarg :history
    :type vector
    :documentation
    "Adjustable vector of entries ordered from oldest to newest.")
   (history-limit
    :initarg :history-limit
    :reader line-editor-history-limit
    :type integer
    :documentation "Maximum number of retained history entries.")
   (history-match-function
    :initarg :history-match-function
    :initform nil
    :reader line-editor-history-match-function
    :type (or null function)
    :documentation
    "Optional function of draft and entry used to filter history traversal.")
   (history-index
    :initform nil
    :documentation
    "Current HISTORY index, or NIL when the editor is not navigating history.")
   (history-stash
    :initform nil
    :documentation
    "Draft saved as the fixed query when history navigation begins.")
   (history-stash-cursor
    :initform nil
    :documentation
    "Cursor saved with HISTORY-STASH, or NIL outside history traversal.")
   (vertical-column
    :initform nil
    :documentation
    "Preferred terminal cell column during repeated vertical movement."))
  (:documentation
   "Portable editable-line state driven by semantic input events."))


;;;; -- State Helpers --

(defun line-editor--normalize-cursor (text cursor)
  "Clamp CURSOR to TEXT and return a complete grapheme boundary."
  (check-type text string)
  (check-type cursor integer)
  (grapheme-boundary-at-or-after
   text
   (min (length text) (max 0 cursor))))

(defun line-editor--make-history (history history-limit)
  "Copy the newest HISTORY entries into an adjustable vector."
  (unless (typep history 'sequence)
    (error 'type-error :datum history :expected-type 'sequence))
  (let* ((length (length history))
         (start  (max 0 (- length history-limit)))
         (copy   (make-array (max 1 (- length start))
                             :adjustable t
                             :fill-pointer 0)))
    (loop for index from start below length
          for entry = (elt history index)
          do (unless (stringp entry)
               (error 'type-error :datum entry :expected-type 'string))
             (vector-push-extend (copy-seq entry) copy))
    copy))

(defun line-editor--leave-history (editor)
  "Leave EDITOR's history traversal and discard its saved draft."
  (setf (slot-value editor 'history-index) nil
        (slot-value editor 'history-stash) nil
        (slot-value editor 'history-stash-cursor) nil)
  nil)

(defun line-editor--set-state (editor text cursor &key leave-history-p)
  "Replace EDITOR's TEXT and CURSOR, optionally leaving history traversal."
  (check-type editor line-editor)
  (check-type text string)
  (when leave-history-p
    (line-editor--leave-history editor))
  (let ((owned-text (copy-seq text)))
    (setf (slot-value editor 'text) owned-text
          (slot-value editor 'cursor)
          (line-editor--normalize-cursor owned-text cursor)
          (slot-value editor 'vertical-column) nil))
  editor)

(defun line-editor--set-cursor (editor cursor)
  "Move EDITOR to CURSOR and end any repeated vertical movement."
  (setf (slot-value editor 'cursor)
        (line-editor--normalize-cursor (line-editor-text editor) cursor)
        (slot-value editor 'vertical-column) nil)
  editor)

(defun line-editor--insert (editor inserted-text)
  "Insert INSERTED-TEXT at EDITOR's cursor."
  (check-type inserted-text string)
  (let* ((text        (line-editor-text editor))
         (cursor      (line-editor-cursor editor))
         (new-text    (concatenate 'string
                                   (subseq text 0 cursor)
                                   inserted-text
                                   (subseq text cursor)))
         (new-cursor  (grapheme-boundary-at-or-after
                       new-text
                       (+ cursor (length inserted-text)))))
    (line-editor--set-state editor new-text new-cursor
                            :leave-history-p t))
  nil)

(defun line-editor--delete-backward (editor)
  "Delete the complete grapheme before EDITOR's cursor."
  (let ((text   (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (if (zerop cursor)
        (line-editor--leave-history editor)
        (let ((start (grapheme-previous-boundary text cursor)))
          (line-editor--set-state
           editor
           (concatenate 'string
                        (subseq text 0 start)
                        (subseq text cursor))
           start
           :leave-history-p t))))
  nil)

(defun line-editor--delete-forward (editor)
  "Delete the complete grapheme at EDITOR's cursor."
  (let ((text   (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (if (>= cursor (length text))
        (line-editor--leave-history editor)
        (let ((end (grapheme-next-boundary text cursor)))
          (line-editor--set-state
           editor
           (concatenate 'string
                        (subseq text 0 cursor)
                        (subseq text end))
           cursor
           :leave-history-p t))))
  nil)

(defun line-editor--whitespace-character-p (character)
  "Return true when CHARACTER is standard line-editor whitespace."
  (case character
    ((#\Space #\Tab #\Newline #\Return #\Page) t)
    (otherwise nil)))

(defun line-editor--word-delimiter-character-p (editor character)
  "Return true when CHARACTER is one of EDITOR's word delimiters."
  (member character (line-editor-word-delimiters editor) :test #'char=))

(defun line-editor--word-separator-p (editor character)
  "Return whether CHARACTER separates words in EDITOR's active editing mode."
  (or (line-editor--whitespace-character-p character)
      (and (line-editor-word-delimiter-mode-p editor)
           (line-editor--word-delimiter-character-p editor character))))

(defun line-editor--word-start (editor)
  "Return the grapheme boundary at the start of the word before EDITOR's cursor."
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (labels ((previous-boundary ()
               (grapheme-previous-boundary text cursor)))
      (loop while (plusp cursor)
            for previous = (previous-boundary)
            while (line-editor--word-separator-p editor (char text previous))
            do (setf cursor previous))
      (loop while (plusp cursor)
            for previous = (previous-boundary)
            while (not (line-editor--word-separator-p
                        editor (char text previous)))
            do (setf cursor previous))
      cursor)))

(defun line-editor--word-end (editor)
  "Return the grapheme boundary at the end of the word after EDITOR's cursor."
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (labels ((next-boundary ()
               (grapheme-next-boundary text cursor)))
      (loop while (< cursor (length text))
            while (line-editor--word-separator-p editor (char text cursor))
            do (setf cursor (next-boundary)))
      (loop while (< cursor (length text))
            while (not (line-editor--word-separator-p editor (char text cursor)))
            do (setf cursor (next-boundary)))
      cursor)))

(defun line-editor--kill-to-end (editor)
  "Delete EDITOR's text after its cursor."
  (line-editor--set-state editor
                          (subseq (line-editor-text editor)
                                  0
                                  (line-editor-cursor editor))
                          (line-editor-cursor editor)
                          :leave-history-p t)
  nil)

(defun line-editor--kill-word (editor)
  "Delete separators and the word immediately before EDITOR's cursor."
  (let* ((text   (line-editor-text editor))
         (cursor (line-editor-cursor editor))
         (start  (line-editor--word-start editor)))
    (line-editor--set-state
     editor
     (concatenate 'string
                  (subseq text 0 start)
                  (subseq text cursor))
     start
     :leave-history-p t))
  nil)

(defun line-editor--kill-word-right (editor)
  "Delete separators and the word immediately after EDITOR's cursor."
  (let* ((text   (line-editor-text editor))
         (cursor (line-editor-cursor editor))
         (end    (line-editor--word-end editor)))
    (line-editor--set-state
     editor
     (concatenate 'string
                  (subseq text 0 cursor)
                  (subseq text end))
     cursor
     :leave-history-p t))
  nil)

(defun line-editor--history-entry-matches-p (editor query entry)
  "True when ENTRY is eligible for EDITOR's fixed history QUERY."
  (let ((function (line-editor-history-match-function editor)))
    (or (zerop (length query))
        (null function)
        (not (null (funcall function query entry))))))

(defun line-editor--history-matching-index (editor query start direction)
  "Find a history entry matching QUERY from START in DIRECTION."
  (let ((history (slot-value editor 'history)))
    (loop for index = start then (+ index direction)
          while (and (<= 0 index) (< index (length history)))
          when (line-editor--history-entry-matches-p
                editor query (aref history index))
            return index)))

(defun line-editor--history-previous (editor)
  "Recall the next older history entry matching EDITOR's saved draft."
  (let* ((history (slot-value editor 'history))
         (index   (slot-value editor 'history-index))
         (query   (if index
                      (slot-value editor 'history-stash)
                      (line-editor-text editor)))
         (match   (line-editor--history-matching-index
                   editor query
                   (if index (1- index) (1- (length history)))
                   -1)))
    (when match
      (unless index
        (setf (slot-value editor 'history-stash) (copy-seq query)
              (slot-value editor 'history-stash-cursor)
              (line-editor-cursor editor)))
      (let ((entry (aref history match)))
        (setf (slot-value editor 'history-index) match)
        (line-editor--set-state editor entry (length entry)))))
  nil)

(defun line-editor--history-next (editor)
  "Recall the next newer match or restore EDITOR's saved draft and cursor."
  (let* ((history (slot-value editor 'history))
         (index   (slot-value editor 'history-index)))
    (when index
      (let* ((draft (slot-value editor 'history-stash))
             (next-index
               (line-editor--history-matching-index
                editor draft (1+ index) 1)))
        (if next-index
            (let ((entry (aref history next-index)))
              (setf (slot-value editor 'history-index) next-index)
              (line-editor--set-state editor entry (length entry)))
            (let ((cursor (slot-value editor 'history-stash-cursor)))
              (line-editor--set-state editor draft cursor)
              (line-editor--leave-history editor))))))
  nil)


;;;; -- Public Editor Operations --

(defun make-line-editor (&key
                           (text "")
                           (cursor (length text))
                           (history #())
                           (history-limit +default-history-limit+)
                           history-match-function
                           (keymap (default-line-editor-keymap))
                           (word-delimiter-mode-p nil)
                           (word-delimiters *default-word-delimiters*))
  "Create an editor initialized with TEXT, CURSOR, and copied HISTORY.

HISTORY is ordered from oldest to newest. HISTORY-MATCH-FUNCTION, when non-NIL,
receives the fixed draft and each candidate entry during traversal. An empty
draft always traverses every entry. CURSOR is clamped to TEXT and advanced when
necessary so that it never divides a grapheme. KEYMAP controls semantic event
dispatch and is retained so callers can update it deliberately.

When WORD-DELIMITER-MODE-P is true, WORD-DELIMITERS separate words for Ctrl-Left,
Ctrl-Right and Ctrl-Backspace behavior. WORD-DELIMITERS defaults to hyphen,
underscore, slash, dot, and colon, and is copied into the editor."
  (check-type text string)
  (unless (and (integerp history-limit) (plusp history-limit))
    (error 'type-error
           :datum history-limit
           :expected-type '(integer 1 *)))
  (unless (or (null history-match-function)
              (functionp history-match-function))
    (error 'type-error
           :datum history-match-function
           :expected-type '(or null function)))
  (check-type keymap keymap)
  (check-type word-delimiter-mode-p boolean)
  (check-type word-delimiters (satisfies line-editor--word-delimiters-p))
  (let* ((owned-text (copy-seq text))
         (safe-cursor (line-editor--normalize-cursor owned-text cursor)))
    (make-instance 'line-editor
                   :text owned-text
                   :cursor safe-cursor
                   :keymap keymap
                   :word-delimiter-mode-p word-delimiter-mode-p
                   :word-delimiters (copy-list word-delimiters)
                   :history (line-editor--make-history history history-limit)
                   :history-limit history-limit
                   :history-match-function history-match-function)))

(defun line-editor-create (&key
                             (text "")
                             (cursor (length text))
                             (history #())
                             (history-limit +default-history-limit+)
                             history-match-function
                             (keymap (default-line-editor-keymap))
                             (word-delimiter-mode-p nil)
                             (word-delimiters *default-word-delimiters*))
  "Create a line editor through the application-oriented named constructor."
  (make-line-editor :text text
                    :cursor cursor
                    :history history
                    :history-limit history-limit
                    :history-match-function history-match-function
                    :keymap keymap
                    :word-delimiter-mode-p word-delimiter-mode-p
                    :word-delimiters word-delimiters))

(defun line-editor-history (editor)
  "Return a detached oldest-to-newest snapshot of EDITOR's history."
  (check-type editor line-editor)
  (line-editor--make-history (slot-value editor 'history)
                             (line-editor-history-limit editor)))

(defun line-editor-set-text (editor text &key (cursor (length text)))
  "Replace EDITOR's text, place its cursor, and leave history traversal."
  (line-editor--set-state editor text cursor :leave-history-p t))

(defun line-editor-clear (editor)
  "Clear EDITOR's text and leave history traversal."
  (line-editor--set-state editor "" 0 :leave-history-p t))

(defun line-editor-add-history (editor text)
  "Append non-empty TEXT to EDITOR's history unless it repeats the newest entry."
  (check-type editor line-editor)
  (check-type text string)
  (let ((history (slot-value editor 'history)))
    (when (and (plusp (length text))
               (or (zerop (length history))
                   (not (string= text
                                 (aref history (1- (length history)))))))
      (vector-push-extend (copy-seq text) history)
      (when (> (length history) (line-editor-history-limit editor))
        (replace history history :start2 1)
        (decf (fill-pointer history)))))
  (line-editor--leave-history editor)
  editor)

(defun line-editor-toggle-word-delimiter-mode (editor)
  "Toggle word-delimiter mode in EDITOR and return its new state."
  (check-type editor line-editor)
  (setf (slot-value editor 'word-delimiter-mode-p)
        (not (line-editor-word-delimiter-mode-p editor)))
  (line-editor-word-delimiter-mode-p editor))

(defun line-editor--text-event-p (event kind)
  "Return true when EVENT carries one string for compound event KIND."
  (and (consp event)
       (eq (first event) kind)
       (consp (rest event))
       (null (rest (rest event)))
       (stringp (second event))))

(defun line-editor--require-event-text (event kind)
  "Return the text carried by EVENT, requiring compound event KIND."
  (unless (line-editor--text-event-p event kind)
    (error 'type-error
           :datum event
           :expected-type `(cons (eql ,kind) (cons string null))))
  (second event))

(defun line-editor--continue-action ()
  "Return the standard action for an editing command that remains active."
  (values :continue nil))

(defun line-editor-command-for-event (editor event)
  "Return EDITOR's command for EVENT and whether a binding was found.

Exact compound-event bindings take precedence over bindings for their event
head. The editor's keymap parent chain supplies fallback bindings."
  (check-type editor line-editor)
  (keymap-lookup (line-editor-keymap editor) event))

(defun line-editor-execute-command (editor command event)
  "Execute COMMAND for original EVENT in EDITOR and return action and payload.

Standard keyword commands implement Clinedi editing behavior. A function or a
non-keyword fbound symbol is called with EDITOR and EVENT and controls both
return values. NIL and :IGNORED are no-op commands returning :IGNORED."
  (check-type editor line-editor)
  (cond
    ((null command)
     (values :ignored nil))
    ((functionp command)
     (funcall command editor event))
    ((and (symbolp command)
          (not (keywordp command)))
     (unless (fboundp command)
       (error 'undefined-function :name command))
     (funcall command editor event))
    ((keywordp command)
     (case command
       (:insert
        (line-editor--insert
         editor (line-editor--require-event-text event :insert))
        (line-editor--continue-action))
       (:paste
        (line-editor--insert
         editor (line-editor--require-event-text event :paste))
        (line-editor--continue-action))
       (:insert-newline
        (line-editor--insert editor (string #\newline))
        (line-editor--continue-action))
       (:line
        (line-editor-set-text
         editor (line-editor--require-event-text event :line))
        (line-editor-execute-command editor :submit event))
       (:left
        (when (plusp (line-editor-cursor editor))
          (line-editor--set-cursor
           editor
           (grapheme-previous-boundary
            (line-editor-text editor)
            (line-editor-cursor editor))))
        (line-editor--continue-action))
       (:right
        (when (< (line-editor-cursor editor)
                 (length (line-editor-text editor)))
          (line-editor--set-cursor
           editor
           (grapheme-next-boundary
            (line-editor-text editor)
            (line-editor-cursor editor))))
        (line-editor--continue-action))
       (:word-left
        (line-editor--set-cursor editor (line-editor--word-start editor))
        (line-editor--continue-action))
       (:word-right
        (line-editor--set-cursor editor (line-editor--word-end editor))
        (line-editor--continue-action))
       (:toggle-word-delimiter-mode
        (line-editor-toggle-word-delimiter-mode editor)
        (line-editor--continue-action))
       (:home
        (line-editor--set-cursor editor 0)
        (line-editor--continue-action))
       (:end
        (line-editor--set-cursor editor (length (line-editor-text editor)))
        (line-editor--continue-action))
       (:backspace
        (line-editor--delete-backward editor)
        (line-editor--continue-action))
       (:delete
        (line-editor--delete-forward editor)
        (line-editor--continue-action))
       (:history-previous
        (line-editor--history-previous editor)
        (line-editor--continue-action))
       (:history-next
        (line-editor--history-next editor)
        (line-editor--continue-action))
       (:kill-to-end
        (line-editor--kill-to-end editor)
        (line-editor--continue-action))
       (:kill-line
        (line-editor-clear editor)
        (line-editor--continue-action))
       (:kill-word
        (line-editor--kill-word editor)
        (line-editor--continue-action))
       (:kill-word-right
        (line-editor--kill-word-right editor)
        (line-editor--continue-action))
       (:complete
        (values :complete nil))
       (:complete-previous
        (values :complete-previous nil))
       (:up
        (values :up nil))
       (:down
        (values :down nil))
       (:submit
        (let ((submitted (copy-seq (line-editor-text editor))))
          (line-editor-add-history editor submitted)
          (line-editor-clear editor)
          (values :submit submitted)))
       (:interrupt
        (values :interrupt nil))
       (:end-of-input
        (cond
          ((zerop (length (line-editor-text editor)))
           (values :end-of-input nil))
          ((< (line-editor-cursor editor)
              (length (line-editor-text editor)))
           (line-editor--delete-forward editor)
           (line-editor--continue-action))
          (t
           (line-editor--continue-action))))
       (:stream-end
        (values :end-of-input nil))
       (:escape
        (values :escape nil))
       (:clear-screen
        (values :clear-screen nil))
       ((:ignore :ignored)
        (values :ignored nil))
       (otherwise
        (error 'type-error
               :datum command
               :expected-type '(member
                                :insert :paste :insert-newline :line
                                :left :right :word-left :word-right
                                :toggle-word-delimiter-mode :home :end
                                :backspace :delete :history-previous :history-next
                                :kill-to-end :kill-line :kill-word
                                :kill-word-right
                                :complete :complete-previous :up :down :submit
                                :interrupt :end-of-input :stream-end :escape
                                :clear-screen :ignore :ignored)))))
    (t
     (error 'type-error
            :datum command
            :expected-type '(or function symbol)))))

(defun line-editor-handle-event (editor event)
  "Resolve and apply semantic EVENT, returning an action and optional payload.

Editing commands return (VALUES :CONTINUE NIL). Submission returns the full
line as its payload and clears the buffer. UI commands such as completion are
returned to the caller without modifying the buffer. :UP and :DOWN support
layout-aware vertical movement. :END-OF-INPUT applies Ctrl-D semantics, while
:STREAM-END returns the :END-OF-INPUT action without clearing buffered text."
  (check-type editor line-editor)
  (line-editor-execute-command
   editor (line-editor-command-for-event editor event) event))

(defun line-editor-render (editor &key suggestion)
  "Return EDITOR's display text and its cursor's terminal cell offset.

When SUGGESTION is a string and the cursor is at the end, append it as an
unaccepted display suffix.  The suggestion does not affect editor state or
the returned cursor offset."
  (check-type editor line-editor)
  (when suggestion
    (check-type suggestion string))
  (let* ((text       (line-editor-text editor))
         (cursor     (line-editor-cursor editor))
         (display    (if (and suggestion (= cursor (length text)))
                         (concatenate 'string text suggestion)
                         (copy-seq text))))
    (values display (text-cell-width text :end cursor))))
