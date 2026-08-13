;;;; -- Blocking editor tests --

(in-package #:clinedi/tests)

(defun terminal-editor-test--size ()
  "Return fixed test terminal dimensions."
  (values 24 80))

(defun terminal-editor-test--read (input &rest arguments)
  "Call EDIT-LINE with INPUT and return its values, output and restore count."
  (let ((restores 0))
    (with-input-from-string (input-stream input)
      (let ((output-stream (make-string-output-stream)))
        (multiple-value-bind (line kind)
            (apply #'edit-line
                   "preamble
> "
                   :input-stream input-stream
                   :output-stream output-stream
                   :terminal-size-function #'terminal-editor-test--size
                   :restore-function (lambda () (incf restores))
                   arguments)
          (values line kind
                  (get-output-stream-string output-stream)
                  restores))))))

(defun terminal-editor-test--render-frames (output)
  "Return the complete cursor-hidden redraw frames in OUTPUT."
  (let ((hide (ansi-cursor-hide))
        (show (ansi-cursor-show)))
    (loop with offset = 0
          for start = (search hide output :start2 offset)
          while start
          for show-start = (search show output
                                   :start2 (+ start (length hide)))
          while show-start
          for end = (+ show-start (length show))
          collect (subseq output start end)
          do (setf offset end))))

(defun run-terminal-editor-tests ()
  "Run blocking frontend and callback regression tests."
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "plain~%")
       :raw-mode-function (lambda () nil))
    (check-equal "plain fallback line" "plain" line)
    (check-equal "plain fallback kind" :line kind)
    (check-equal "plain fallback prompt"
                 (format nil "preamble~%~c> " #\return)
                 output)
    (check-equal "plain fallback restores terminal" 1 restores))
  (let ((before-count 0)
        (after-count 0))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "plain~%")
         :raw-mode-function (lambda () nil)
         :before-prompt-function
         (lambda (stream)
           (incf before-count)
           (write-string "<before>" stream))
         :after-prompt-function
         (lambda (stream)
           (incf after-count)
           (write-string "<after>" stream)))
      (declare (ignore line kind restores))
      (check-equal "fallback prompt boundary callbacks run once"
                   (format nil "preamble~%~c<before>> <after>" #\return)
                   output)
      (check-equal "fallback before prompt callback count" 1 before-count)
      (check-equal "fallback after prompt callback count" 1 after-count)))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "abc~%")
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (check-equal "raw editor preserves submit payload" "abc" line)
    (check-equal "raw editor submit kind" :line kind)
    (check-true "raw editor renders entered text" (search "abc" output))
    (check-equal "raw editor restores terminal" 1 restores))
  (let ((before-count 0)
        (after-count 0))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "~c[12~%" (code-char 27))
         :raw-mode-function (lambda () t)
         :keyboard-enhancement-p nil
         :bracketed-paste-p nil
         :before-prompt-function
         (lambda (stream)
           (incf before-count)
           (write-string "<before>" stream))
         :after-prompt-function
         (lambda (stream)
           (incf after-count)
           (write-string "<after>" stream)))
      (declare (ignore line kind restores))
      (check-equal "redraw before prompt callback count" 1 before-count)
      (check-equal "redraw after prompt callback count" 1 after-count)
      (check-equal "redraw keeps one before marker"
                   1
                   (loop with start = 0
                         for position = (search "<before>" output :start2 start)
                         while position
                         count 1
                         do (setf start (+ position 8))))
      (check-equal "redraw keeps one after marker"
                   1
                   (loop with start = 0
                         for position = (search "<after>" output :start2 start)
                         while position
                         count 1
                         do (setf start (+ position 7))))))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "first~c[13;2usecond~%" (code-char 27))
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore restores))
    (check-equal "enhanced shift-enter inserts a logical line"
                 (format nil "first~%second")
                 line)
    (check-equal "enhanced multiline input still submits" :line kind)
    (check-true "raw editor enables keyboard enhancement"
                (search (format nil "~c[>1u" (code-char 27)) output))
    (check-true "raw editor restores keyboard reporting"
                (search (format nil "~c[<u" (code-char 27)) output)))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (string #\newline)
       :raw-mode-function (lambda () t)
       :keyboard-enhancement-p nil)
    (declare (ignore line kind restores))
    (check-true "raw editor enables bracketed paste"
                (search (format nil "~c[?2004h" (code-char 27)) output))
    (check-true "raw editor disables bracketed paste"
                (search (format nil "~c[?2004l" (code-char 27)) output)))
  (let ((size-calls 0)
        (restores 0))
    (flet ((changing-size ()
             ;; The first three typed frames use eight columns. The width
             ;; changes while the fourth input event is being read.
             (incf size-calls)
             (values 24 (if (<= size-calls 8) 8 4))))
      (with-input-from-string (input-stream (format nil "abcd~%"))
        (let ((output-stream (make-string-output-stream)))
          (multiple-value-bind (line kind)
              (edit-line
               (format nil "preamble~%> ")
               :input-stream input-stream
               :output-stream output-stream
               :terminal-size-function #'changing-size
               :raw-mode-function (lambda () t)
               :restore-function (lambda () (incf restores))
               :suggestion-function
               (lambda (text history)
                 (declare (ignore history))
                 (and (plusp (length text))
                      (string= text "abcdefgh" :end2 (length text))
                      "abcdefgh"))
               :bracketed-paste-p nil)
            (let* ((frames
                     (terminal-editor-test--render-frames
                      (get-output-stream-string output-stream)))
                   (resized-frame (fifth frames))
                   (resized-prefix
                     (concatenate 'string
                                  (ansi-cursor-hide)
                                  (ansi-cursor-up 1)
                                  (ansi-cursor-column 2))))
              (check-equal "resized editor preserves submitted text"
                           "abcd" line)
              (check-equal "resized editor submit kind" :line kind)
              (check-equal "resize test redraw frame count" 6
                           (length frames))
              (check-true "terminal size is refreshed while editing"
                          (>= size-calls 9))
              (check-true
               "resize redraw uses the reflowed prior cursor row"
               (and resized-frame
                    (<= (length resized-prefix) (length resized-frame))
                    (string= resized-prefix resized-frame
                             :end2 (length resized-prefix))))
              (check-true "resize redraw retains the wrapped suggestion"
                          (and resized-frame
                               (search (ansi-colorize "efgh" :bright-black)
                                       resized-frame)))
              (check-equal "resized editor restores terminal" 1
                           restores)))))))
  (check-equal "completion budget follows soft-wrapped editor rows"
               1
               (clinedi::terminal-completion--row-budget
                "aaa aaa aaa" 0 6 4))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "first~c~csecond~%" (code-char 27) #\return)
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "modified enter inserts a logical line"
                 (format nil "first~%second")
                 line)
    (check-equal "multiline input still submits with enter" :line kind))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil
               "abcd~c~cxy~c~cabcdef~c[A!~%"
               (code-char 27) #\return
               (code-char 27) #\return
               (code-char 27))
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "Up edits the preceding visual line"
                 (format nil "abcd~%xy!~%abcdef")
                 line)
    (check-equal "vertically edited input remains submittable" :line kind))
  (with-input-from-string
      (input-stream (format nil "say user~c[A!~%" (code-char 27)))
    (let ((output-stream (make-string-output-stream)))
      (multiple-value-bind (line kind)
          (edit-line "> "
                     :input-stream input-stream
                     :output-stream output-stream
                     :terminal-size-function (lambda () (values 24 8))
                     :raw-mode-function (lambda () t)
                     :restore-function (lambda () nil)
                     :bracketed-paste-p nil)
        (let ((output (get-output-stream-string output-stream)))
          (check-equal "Up edits a soft word-wrapped row"
                       "sa!y user"
                       line)
          (check-equal "soft-wrapped input remains submittable" :line kind)
          (check-true "blocking editor emits the soft word break"
                      (search (format nil "~a~%~cuser"
                                      (ansi-clear-line-right)
                                      #\return)
                              output))))))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "~c[A~%" (code-char 27))
       :history #("older")
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "Up beyond the first visual row recalls history" "older" line)
    (check-equal "history fallback remains submittable" :line kind))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "log~c[A~%" (code-char 27))
       :history #("git log --oneline" "echo newer")
       :history-match-function (lambda (query entry)
                                 (search query entry))
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "Up recalls the newest matching history entry"
                 "git log --oneline" line)
    (check-equal "filtered history fallback remains submittable" :line kind))
  (let ((keymap (default-line-editor-keymap)))
    (keymap-bind keymap :up :left)
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "ab~c[A!~%" (code-char 27))
         :keymap keymap
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil)
      (declare (ignore output restores))
      (check-equal "remapped Up bypasses vertical movement" "a!b" line)
      (check-equal "remapped vertical input remains submittable" :line kind)))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       "partial"
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "physical EOF discards partial raw input" nil line)
    (check-equal "physical EOF result kind" :eof kind))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "~c~%" #\tab)
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil
       :completion-function
       (lambda (text cursor)
         (declare (ignore text cursor))
         (values 0 '("print") '("print")))
       :completion-accept-function
       (lambda (candidate)
         (concatenate 'string candidate " ")))
    (declare (ignore output restores))
    (check-equal "completion callback replacement" "print " line)
    (check-equal "completion callback submit kind" :line kind))
  (flet ((complete (text cursor)
           (declare (ignore text))
           (values (- cursor 3)
                   '("print" "printf" "private")
                   '("PRINT" "PRINTF" "PRIVATE")))

         (accept (candidate)
           (concatenate 'string candidate " ")))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c!~%" #\tab #\tab)
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore restores))
      (check-equal "Tab cycles live completion candidates"
                   "printf !"
                   line)
      (check-equal "typed input submits after retaining selection" :line kind)
      (check-true "live completion renders supplied labels"
                  (search "PRINTF" output)))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[Z!~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "Shift-Tab cycles live completions backward"
                   "private !"
                   line)
      (check-equal "backward completion remains submittable" :line kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[C~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "right arrow navigates completion grid" "printf " line)
      (check-equal "Enter submits the selected grid completion" :line kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[C~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept
         :completion-arrangement :vertical)
      (declare (ignore output restores))
      (check-equal "vertical completion ignores horizontal navigation"
                   "print "
                   line)
      (check-equal "vertical completion submits selected candidate"
                   :line
                   kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~cX~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "Escape restores text preceding completion" "pri" line)
      (check-equal "restored completion text remains submittable" :line kind)))
  (let ((keymap (default-line-editor-keymap)))
    (keymap-bind keymap :right :complete-previous)
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[C~%" #\tab (code-char 27))
         :keymap keymap
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function
         (lambda (text cursor)
           (declare (ignore text))
           (values (- cursor 3) '("print" "printf" "private")))
         :completion-accept-function
         (lambda (candidate)
           (concatenate 'string candidate " ")))
      (declare (ignore output restores))
      (check-equal "completion selector honors a remapped Right command"
                   "private " line)
      (check-equal "remapped completion remains submittable" :line kind)))
  (let ((source-history (vector "older"))
        (callback-histories nil))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "a~c[C~%" (code-char 27))
         :history source-history
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :suggestion-function
         (lambda (text history)
           (push history callback-histories)
           (and (string= text "a") "abc")))
      (declare (ignore output restores))
      (check-equal "right accepts suggestion" "abc" line)
      (check-equal "suggestion submit kind" :line kind))
    (check-true "suggestion callback reuses editor-owned history"
                (and callback-histories
                     (every (lambda (history)
                              (eq history (first callback-histories)))
                            callback-histories)))
    (check-true "suggestion callback cannot mutate caller history"
                (not (eq source-history (first callback-histories)))))
  (let ((keymap (default-line-editor-keymap)))
    (keymap-bind keymap :right :left)
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "a~c[C!~%" (code-char 27))
         :keymap keymap
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :suggestion-function
         (lambda (text history)
           (declare (ignore history))
           (and (string= text "a") "abc")))
      (declare (ignore output restores))
      (check-equal "suggestion acceptance honors a remapped Right command"
                   "!a" line)
      (check-equal "remapped suggestion input remains submittable" :line kind)))
  (values))
