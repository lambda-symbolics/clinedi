;;;; -- Blocking terminal editor --

(in-package #:clinedi)

(defun terminal-editor--default-size ()
  "Return conservative terminal dimensions for callers without a backend."
  (values 24 80))

(defun terminal-editor--false ()
  "Return false for the default unavailable raw-mode backend."
  nil)

(defun terminal-editor--no-op ()
  "Perform no operation."
  (values))

(defun terminal-editor--common-prefix (strings)
  "Return the longest common prefix of nonempty STRINGS."
  (when strings
    (reduce (lambda (first second)
              (subseq first 0 (or (mismatch first second) (length first))))
            (rest strings)
            :initial-value (first strings))))

(defun terminal-editor--replace-range (editor start end replacement)
  "Replace EDITOR text from START through END with REPLACEMENT."
  (let ((text (line-editor-text editor)))
    (line-editor-set-text
     editor
     (concatenate 'string
                  (subseq text 0 start)
                  replacement
                  (subseq text end))
     :cursor (+ start (length replacement)))))

(defclass terminal-completion-candidate ()
  ((value
    :initarg :value
    :reader terminal-completion-candidate-value
    :documentation "Opaque value passed to the completion acceptance callback.")
   (display
    :initarg :display
    :reader terminal-completion-candidate-display
    :type string
    :documentation "Sanitized single-line candidate label."))
  (:documentation "One blocking-editor completion candidate and its label."))

(defclass terminal-completion-session ()
  ((selector
    :initarg :selector
    :reader terminal-completion-session-selector
    :type selector
    :documentation "Navigation and viewport state for this completion session.")
   (original-text
    :initarg :original-text
    :reader terminal-completion-session-original-text
    :type string
    :documentation "Editor text before candidate preview began.")
   (replacement-start
    :initarg :replacement-start
    :reader terminal-completion-session-replacement-start
    :type integer
    :documentation "First character index replaced by a candidate.")
   (replacement-end
    :initarg :replacement-end
    :reader terminal-completion-session-replacement-end
    :type integer
    :documentation "Original cursor and exclusive replacement end index.")
   (accept-function
    :initarg :accept-function
    :reader terminal-completion-session-accept-function
    :type function
    :documentation "Function converting a candidate value to replacement text."))
  (:documentation
   "Transient completion selection for the blocking line editor."))

(defun terminal-completion--candidates (candidates displays)
  "Return internal completion candidates for CANDIDATES and DISPLAYS."
  (let ((remaining-displays (and displays (coerce displays 'list))))
    (loop for candidate in candidates
          for raw-display = (if remaining-displays
                                (pop remaining-displays)
                                candidate)
          for display = (sanitize-text
                         (if (stringp raw-display)
                             raw-display
                             (princ-to-string raw-display))
                         :single-line-p t)
          collect (make-instance 'terminal-completion-candidate
                                 :value candidate
                                 :display display))))

(defun terminal-completion--make-session
    (editor start end candidates displays accept-function arrangement)
  "Create a completion session for EDITOR and preview its first candidate."
  (let* ((items (terminal-completion--candidates candidates displays))
         (selector (make-selector :items items
                                  :visible-count (max 1 (length items))
                                  :arrangement arrangement))
         (session
           (make-instance
            'terminal-completion-session
            :selector selector
            :original-text (copy-seq (line-editor-text editor))
            :replacement-start start
            :replacement-end end
            :accept-function accept-function)))
    (terminal-completion--preview
     editor session (selector-selected-item selector))
    session))

(defun terminal-completion--preview (editor session candidate)
  "Preview CANDIDATE in EDITOR using SESSION's original replacement range."
  (when candidate
    (let* ((text (terminal-completion-session-original-text session))
           (start (terminal-completion-session-replacement-start session))
           (end (terminal-completion-session-replacement-end session))
           (replacement
             (funcall (terminal-completion-session-accept-function session)
                      (terminal-completion-candidate-value candidate))))
      (check-type replacement string)
      (line-editor-set-text
       editor
       (concatenate 'string
                    (subseq text 0 start)
                    replacement
                    (subseq text end))
       :cursor (+ start (length replacement)))))
  editor)

(defun terminal-completion--restore (editor session)
  "Restore EDITOR to the state preceding SESSION and return EDITOR."
  (line-editor-set-text
   editor
   (terminal-completion-session-original-text session)
   :cursor (terminal-completion-session-replacement-end session)))

(defun terminal-completion--candidate-width (candidate columns)
  "Return CANDIDATE's selection-cell width capped at COLUMNS."
  (min columns
       (+ 2 (text-cell-width
             (terminal-completion-candidate-display candidate)))))

(defun terminal-completion--cell-strings
    (candidate selected-p width)
  "Return plain and presented WIDTH-cell strings for CANDIDATE."
  (let* ((label-width (max 0 (- width 2)))
         (label (text-cell-prefix
                 (terminal-completion-candidate-display candidate)
                 label-width))
         (content (concatenate 'string
                               (if selected-p "▸ " "  ")
                               label))
         (padding (make-string
                   (max 0 (- width (text-cell-width content)))
                   :initial-element #\space))
         (plain (concatenate 'string content padding))
         (presented (concatenate 'string
                                 (if selected-p
                                     (ansi-reverse-video content)
                                     content)
                                 padding)))
    (values plain presented)))

(defun terminal-completion--arrange (session columns row-budget)
  "Arrange SESSION within COLUMNS and ROW-BUDGET and return rows and widths."
  (let ((selector (terminal-completion-session-selector session))
        (layout-columns (max 1 (1- columns))))
    (labels ((arrange ()
               (selector-arrange
                selector layout-columns
                :width-function
                (lambda (candidate)
                  (terminal-completion--candidate-width
                   candidate layout-columns)))))
      (multiple-value-bind (index-rows column-widths)
          (arrange)
        (when (> (length index-rows) row-budget)
          (setf (slot-value selector 'visible-count)
                (max 1 (* row-budget
                          (selector-column-count selector))))
          (multiple-value-setq (index-rows column-widths) (arrange)))
        (values index-rows column-widths)))))

(defun terminal-completion--footer (session columns row-budget)
  "Return plain and presented selector footer strings for SESSION."
  (let ((selector (terminal-completion-session-selector session)))
    (multiple-value-bind (index-rows column-widths)
        (terminal-completion--arrange session columns row-budget)
      (let ((plain (make-string-output-stream))
            (presented (make-string-output-stream)))
        (loop for index-row in index-rows
              for row-index from 0
              do (when (plusp row-index)
                   (write-char #\newline plain)
                   (write-char #\newline presented))
                 (loop for item-index in index-row
                       for column-index from 0
                       for candidate = (nth item-index
                                            (selector-items selector))
                       for width = (nth column-index column-widths)
                       do (when (plusp column-index)
                            (write-string "  " plain)
                            (write-string "  " presented))
                          (multiple-value-bind (plain-cell presented-cell)
                              (terminal-completion--cell-strings
                               candidate
                               (= item-index (selector-selection selector))
                               width)
                            (write-string plain-cell plain)
                            (write-string presented-cell presented))))
        (values (get-output-stream-string plain)
                (get-output-stream-string presented))))))

(defun terminal-completion--row-budget
    (text prompt-width columns terminal-rows)
  "Return the footer row budget beneath editor TEXT."
  (let ((layout (screen--editor-layout text prompt-width columns)))
    (max 1
         (- terminal-rows
            (1+ (screen-editor-layout-end-row layout))))))

(defun terminal-completion--handle-command (editor session command)
  "Handle resolved COMMAND in SESSION and return session and forwarding flag."
  (multiple-value-bind (action candidate)
      (selector-handle-event
       (terminal-completion-session-selector session)
       command)
    (case action
      (:changed
       (terminal-completion--preview editor session candidate)
       (values session nil))
      ((:accept :dismiss)
       (terminal-completion--preview editor session candidate)
       (values nil t))
      (:cancel
       (terminal-completion--restore editor session)
       (values nil (not (eq command :escape))))
      (:unhandled
       (values session t)))))

(defun terminal-editor--suggestion (editor suggestion-function)
  "Return a valid full-text suggestion for EDITOR, or NIL."
  (when (and suggestion-function
             (= (line-editor-cursor editor)
                (length (line-editor-text editor))))
    (let* ((text (line-editor-text editor))
           (suggestion
             ;; The editor already owns this copied history vector. Passing it
             ;; directly avoids duplicating every entry on each keystroke.
             (funcall suggestion-function
                      text
                      (slot-value editor 'history))))
      (when (and (stringp suggestion)
                 (> (length suggestion) (length text))
                 (string= text suggestion :end2 (length text)))
        suggestion))))

(defun terminal-editor--complete
    (editor &key completion-function common-prefix-function
                 completion-accept-function completion-arrangement stream)
  "Apply completion or return a new interactive completion session."
  (unless completion-function
    (write-char (code-char 7) stream)
    (force-output stream)
    (return-from terminal-editor--complete nil))
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (multiple-value-bind (start candidates displays)
        (funcall completion-function text cursor)
      (cond ((or (null candidates)
                 (not (integerp start))
                 (minusp start)
                 (> start cursor))
             (write-char (code-char 7) stream)
             (force-output stream)
             nil)
            ((null (rest candidates))
             (let* ((candidate (first candidates))
                    (replacement
                      (funcall completion-accept-function candidate)))
               (terminal-editor--replace-range
                editor start cursor replacement)
               nil))
            (t
             (let* ((common (funcall common-prefix-function candidates))
                    (prefix-length (- cursor start)))
               (if (> (length common) prefix-length)
                   (progn
                     (terminal-editor--replace-range editor start cursor common)
                     nil)
                   (terminal-completion--make-session
                    editor start cursor candidates displays
                    completion-accept-function completion-arrangement))))))))

(defun terminal-editor--finish
    (editor kind &key payload prompt-width columns previous-row
                      highlight-function stream)
  "Finish EDITOR input of KIND and return the blocking API values."
  (ecase kind
    (:submit
     (render--finish-line
      payload
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :highlight-function highlight-function
      :stream stream)
     (values payload :line))
    (:interrupt
     (render--finish-line
      (line-editor-text editor)
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :marker "^C"
      :highlight-function highlight-function
      :stream stream)
     (values nil :abort))
    (:end-of-input
     (render--finish-line
      (line-editor-text editor)
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :highlight-function highlight-function
      :stream stream)
     (values nil :eof))))

(defun terminal-editor--fallback
    (prompt input-stream output-stream before-prompt-function after-prompt-function)
  "Read a plain line after PROMPT when raw mode is unavailable."
  (when before-prompt-function
    (funcall before-prompt-function output-stream))
  (write-string prompt output-stream)
  (when after-prompt-function
    (funcall after-prompt-function output-stream))
  (force-output output-stream)
  (let ((line (cl:read-line input-stream nil nil)))
    (if line
        (values line :line)
        (values nil :eof))))

(defun edit-line
    (prompt &key (history #())
                 history-match-function
                 (input-stream *standard-input*)
                 (output-stream *standard-output*)
                 (terminal-size-function #'terminal-editor--default-size)
                 (raw-mode-function #'terminal-editor--false)
                 (restore-function #'terminal-editor--no-op)
                 (highlight-function #'identity)
                 completion-function
                 (common-prefix-function #'terminal-editor--common-prefix)
                 (completion-accept-function #'identity)
                 (completion-arrangement :grid)
                 suggestion-function
                 (keymap (default-line-editor-keymap))
                 (keyboard-enhancement-p t)
                 (bracketed-paste-p t)
                 before-prompt-function
                 after-prompt-function)
  "Edit one line under PROMPT and return line and result kind.

The result kind is :LINE, :ABORT or :EOF. HISTORY is copied into an incremental
LINE-EDITOR. HISTORY-MATCH-FUNCTION optionally filters traversal by the fixed
draft present when it begins; it receives that draft and each candidate entry.
An empty draft always traverses all entries. Terminal ownership remains with
the caller through size, raw-mode and restore callbacks. Highlighting,
completion and suggestion callbacks add application policy without coupling
Clinedi to a parser or history store.
KEYMAP supplies programmable event-to-command bindings to the incremental
editor and is retained for the duration of this call.
The size callback is refreshed between redraws and input events so a resized
terminal can reflow the last visible frame without displacing the cursor.
COMPLETION-ARRANGEMENT is :GRID for a width-measured row-major selector or
:VERTICAL for one completion per row. While a selector is open, arrows
navigate, Tab and Shift-Tab cycle forward and backward, Escape restores the
uncompleted text, and other input keeps the selected completion before
applying that input.

KEYBOARD-ENHANCEMENT-P requests CSI-u and modifyOtherKeys reporting while raw
mode is active, allowing modified Enter and other modified keys to remain
distinguishable from their unmodified forms. BRACKETED-PASTE-P balances terminal
bracketed paste mode around the same raw editing region.

BEFORE-PROMPT-FUNCTION and AFTER-PROMPT-FUNCTION, when supplied, receive the
output stream immediately around the initial prompt write. They are not called
for redraws, so applications can emit one-shot trusted prompt boundary controls.

When raw mode is unavailable, this function invokes the same boundary callbacks,
prints the final prompt line and uses ordinary READ-LINE."
  (unless (member completion-arrangement '(:vertical :grid))
    (error 'type-error
           :datum completion-arrangement
           :expected-type '(member :vertical :grid)))
  (multiple-value-bind (preamble editable-prompt)
      (split-prompt prompt)
    (multiple-value-bind (rows columns)
        (funcall terminal-size-function)
      (setf rows (max 1 rows)
            columns (max 1 columns))
      (let ((editor (make-line-editor
                     :history history
                     :history-match-function history-match-function
                     :keymap keymap))
            (prompt-width (ansi-display-width editable-prompt))
            (previous-row 0)
            (rendered-text "")
            (rendered-cursor 0)
            (completion nil)
            (raw-p nil))
        (write-display preamble :stream output-stream)
        (unwind-protect
             (progn
               (setf raw-p (funcall raw-mode-function))
              (unless raw-p
                (return-from edit-line
                  (terminal-editor--fallback
                   editable-prompt input-stream output-stream
                   before-prompt-function after-prompt-function)))
               (when keyboard-enhancement-p
                 (enable-keyboard-enhancement :stream output-stream))
               (when bracketed-paste-p
                 (enable-bracketed-paste :stream output-stream))
              (when before-prompt-function
                (funcall before-prompt-function output-stream))
               (setf previous-row
                     (render--write-prompt editable-prompt prompt-width columns
                                           output-stream))
              (when after-prompt-function
                (funcall after-prompt-function output-stream))
               (labels ((refresh-terminal-size ()
                          (multiple-value-bind (next-rows next-columns)
                              (funcall terminal-size-function)
                            (setf next-rows (max 1 next-rows)
                                  next-columns (max 1 next-columns))
                            (unless (= next-columns columns)
                              ;; The terminal reflows the frame that is
                              ;; already visible. Recompute where that old
                              ;; frame left the physical cursor before using
                              ;; the new width to paint the next frame.
                              (multiple-value-bind
                                    (row column pending-wrap)
                                  (screen-position
                                   rendered-text
                                   :prompt-width prompt-width
                                   :columns next-columns
                                   :end rendered-cursor)
                                (declare (ignore column pending-wrap))
                                (setf previous-row row)))
                            (setf rows next-rows
                                  columns next-columns)))
                        (remember-rendered-frame ()
                          (let* ((layout
                                   (screen--editor-layout
                                    (line-editor-text editor)
                                    prompt-width columns))
                                 (display-indexes
                                   (screen-editor-layout-display-indexes
                                    layout)))
                            (setf rendered-text
                                  (screen-editor-layout-display layout)
                                  rendered-cursor
                                  (aref display-indexes
                                        (line-editor-cursor editor)))))
                        (vertical-command (command)
                          (case command
                            (:up
                             (unless (line-editor-move-vertical
                                      editor -1
                                      :columns columns
                                      :prompt-width prompt-width)
                               ':history-previous))
                            (:down
                             (unless (line-editor-move-vertical
                                      editor 1
                                      :columns columns
                                      :prompt-width prompt-width)
                               ':history-next))
                            (t
                             command)))
                        (handle-editor-command (event command)
                          (let ((effective-command
                                  (vertical-command command)))
                            (when effective-command
                              (multiple-value-bind (action payload)
                                  (line-editor-execute-command
                                   editor effective-command event)
                                (case action
                                  ((:submit :interrupt :end-of-input)
                                   (return-from edit-line
                                     (terminal-editor--finish
                                      editor action
                                      :payload payload
                                      :prompt-width prompt-width
                                      :columns columns
                                      :previous-row previous-row
                                      :highlight-function highlight-function
                                      :stream output-stream)))
                                  ((:complete :complete-previous)
                                   (let ((next-completion
                                           (terminal-editor--complete
                                            editor
                                            :completion-function
                                            completion-function
                                            :common-prefix-function
                                            common-prefix-function
                                            :completion-accept-function
                                            completion-accept-function
                                            :completion-arrangement
                                            completion-arrangement
                                            :stream output-stream)))
                                     (when (and next-completion
                                                (eq action
                                                    :complete-previous))
                                       (let ((selector
                                               (terminal-completion-session-selector
                                                next-completion)))
                                         (selector-move selector -1)
                                         (terminal-completion--preview
                                          editor next-completion
                                          (selector-selected-item selector))))
                                     (setf completion next-completion)))
                                  (:clear-screen
                                   (refresh-terminal-size)
                                   (write-string (ansi-clear-screen)
                                                 output-stream)
                                   (write-display preamble
                                                  :stream output-stream)
                                   (setf previous-row
                                         (render--write-prompt
                                          editable-prompt prompt-width columns
                                          output-stream)
                                         rendered-text ""
                                         rendered-cursor 0))
                                  ((:continue :escape :ignored)
                                   nil)))))))
                 (loop
                   (refresh-terminal-size)
                   (let* ((suggestion
                            (and (null completion)
                                 (terminal-editor--suggestion
                                  editor suggestion-function)))
                          (suffix
                            (and suggestion
                                 (subseq suggestion
                                         (line-editor-cursor editor))))
                          (footer-text nil)
                          (footer-display nil))
                     (when completion
                       (multiple-value-setq (footer-text footer-display)
                         (terminal-completion--footer
                          completion
                          columns
                          (terminal-completion--row-budget
                           (line-editor-text editor)
                           prompt-width columns rows))))
                     (setf previous-row
                           (render-line
                            (line-editor-text editor)
                            :cursor (line-editor-cursor editor)
                            :prompt-width prompt-width
                            :columns columns
                            :previous-row previous-row
                            :suggestion suffix
                            :footer-text footer-text
                            :footer-display footer-display
                            :highlight-function highlight-function
                            :stream output-stream))
                     (remember-rendered-frame)
                     (let ((event (read-event :stream input-stream)))
                       ;; A resize commonly happens while READ-EVENT is
                       ;; blocked. Refresh now so submit and clear-screen use
                       ;; the geometry of the frame the user can see.
                       (refresh-terminal-size)
                       (cond ((eq event :ignore)
                              nil)
                             (t
                              (let ((command
                                      (line-editor-command-for-event
                                       editor event)))
                                (cond
                                  (completion
                                   (multiple-value-bind
                                         (next-completion forward-command-p)
                                       (terminal-completion--handle-command
                                        editor completion command)
                                     (setf completion next-completion)
                                     (when forward-command-p
                                       (handle-editor-command event command))))
                                  ((and (eq command :right) suggestion
                                        (= (line-editor-cursor editor)
                                           (length
                                            (line-editor-text editor))))
                                   (line-editor-set-text
                                    editor suggestion
                                    :cursor (length suggestion)))
                                  (t
                                   (handle-editor-command
                                    event command)))))))))))
          (when raw-p
            (when bracketed-paste-p
              (disable-bracketed-paste :stream output-stream))
            (when keyboard-enhancement-p
              (disable-keyboard-enhancement :stream output-stream)))
          (funcall restore-function))))))
