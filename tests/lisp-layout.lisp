(in-package #:clinedi)

(defun run-lisp-layout-tests ()
  "Check one-line Lisp is laid out by structure, and that only whitespace between tokens changes."
  (loop for (source expected)
          in '(("(defparameter *x* 1) (foo) (let ((sources (directory \"src/**/*.lisp\")) (count 0)) (dolist (file sources) (incf count (length (uiop:read-file-lines file)))) count)"
                ("(defparameter *x* 1)"
                 "(foo)"
                 "(let ((sources (directory \"src/**/*.lisp\"))"
                 "      (count 0))"
                 "  (dolist (file sources)"
                 "    (incf count (length (uiop:read-file-lines file))))"
                 "  count)"))
               ("(when (probe-file \"x\") (delete-file \"x\"))"
                ("(when (probe-file \"x\") (delete-file \"x\"))"))
               ("(mapcar (lambda (entry) (let ((name (getf entry :name))) (string-downcase name))) entries)"
                ("(mapcar (lambda (entry)"
                 "          (let ((name (getf entry :name)))"
                 "            (string-downcase name)))"
                 "        entries)"))
               ("(cond ((null x) :none) ((consp x) (first x)) (t x))"
                ("(cond ((null x) :none)"
                 "      ((consp x) (first x))"
                 "      (t x))"))
               ("(loop for file in files when (probe-file file) collect file finally (return nil))"
                ("(loop for file in files"
                 "      when (probe-file file)"
                 "      collect file"
                 "      finally (return nil))"))
               ("(setf (config :web-search-mode) :live (config :knowledge-mode) :off)"
                ("(setf (config :web-search-mode) :live"
                 "      (config :knowledge-mode) :off)"))
               ("(defmethod)" ("(defmethod)"))
               ("(defun square (x) \"Return X squared.\" (* x x))"
                ("(defun square (x)"
                 "  \"Return X squared.\""
                 "  (* x x))"))
               ("(list '(let ((a 1)) a) #(1 2 3) :complete) ; trailing ((   comment"
                ("(list '(let ((a 1)) a) #(1 2 3) :complete) ; trailing ((   comment")))
        do (assert (string= (lisp-source-layout source 100) (format nil "~{~A~^~%~}" expected))
                   () "Layout did not reflect the structure of ~S." source))
  (dolist (case
            (list
             (list "(let ((start (get-internal-real-time))) (list :start start :message \"(literal)   spaces\" :last :complete))"
                   '("\"(literal)   spaces\"" ":complete"))
             (list "(list '#(one two) #'absent-package:item #.(error \"display only\") #1=(shared) #1# #\\( #\\) #\\Space #\\  |symbol (with)   spaces| escaped\\ symbol :complete)"
                   '("'#(one two)" "#'absent-package:item" "#.(error \"display only\")" "#1=(shared)" "#1#"
                     "#\\(" "#\\)" "#\\Space" "#\\ " "|symbol (with)   spaces|" "escaped\\ symbol" ":complete"))
             (list "(list #| comment (with)   spaces #| nested |# |# :first :second :third :complete) ; trailing ((   comment"
                   '("#| comment (with)   spaces #| nested |# |#" "; trailing ((   comment" ":complete"))))
    (destructuring-bind (source literals) case
      (let ((formatted (lisp-source-layout source 48)))
        (assert (and (find #\Newline formatted)
                     (every (lambda (literal) (search literal formatted)) literals)
                     (string= (remove-if (lambda (character) (find character '(#\Space #\Newline)))
                                         formatted)
                              (remove #\Space source)))
                () "Narrow layout did not preserve tokens and break lines for ~S." source))))
  (dolist (source (list (format nil "(let ((value 42))~%    (list :value value))")
                       "(list :incomplete (value" "(list :extra))"))
    (assert (string= source (lisp-source-layout source 16))
            () "Multiline or malformed source was changed: ~S." source))
  (format t "All Clinedi Lisp layout checks passed.~%"))
