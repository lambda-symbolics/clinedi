;;;; -- Source Checks --

;;; Check the project's source with cheque, the Lambda Symbolics source checker.
;;; Arguments go to CHEQUE:MAIN.
;;;
;;; cheque, Mallet, and the libraries only they need are development tools.
;;; checks/qlfile lists them apart from the runtime qlfile, and they install as
;;; a qlot project in the per-user cache, named by a digest of the files in
;;; checks/, never below the checkout: ASDF's default source registry searches
;;; trees such as ~/common-lisp, and would find Mallet's vendored ASDF inside a
;;; .qlot directory there. cheque follows its default branch: a new install
;;; refreshes it at once and later checks at most hourly. The lock pins every
;;; other tool.
;;;
;;; Usage: sbcl --script script/check-source.lisp [--fix] [--no-warnings] [--no-parinfer]

(require :asdf)
;; Only the installed tools provide systems here, so a checkout of cheque or
;; Mallet below ~/common-lisp never stands in for them.
(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))
(require :sb-md5)

(defparameter *check-floating-tools* '("cheque")
  "The development tools that follow their default branch instead of the lock.")

(defparameter *check-refresh-seconds* 3600
  "The longest the floating development tools go without a refresh.")

(defun check--tool-files (root)
  "Return the files in ROOT's checks/ directory, which define its development tools."
  (sort (uiop:directory-files (merge-pathnames "checks/" root))
        #'string<
        :key #'file-namestring))

(defun check--tool-root (root)
  "Return the cached qlot project for ROOT's current development tool definition."
  (let ((digest (sb-md5:md5sum-string
                 (with-output-to-string (stream)
                   (dolist (file (check--tool-files root))
                     (write-string (file-namestring file) stream)
                     (write-char #\Nul stream)
                     (write-string (uiop:read-file-string file) stream)
                     (write-char #\Nul stream)))
                 :external-format ':utf-8)))
    (uiop:xdg-cache-home "cheque/tools/" (format nil "~(~{~2,'0x~}~)/" (coerce digest 'list)))))

(defun check--run-qlot (tools &rest projects)
  "Install the qlot project TOOLS, or update PROJECTS in it, in a child SBCL; return T on success.

The child loads qlot from Quicklisp with the default ASDF configuration, apart
from this process's restricted one."
  (let ((form `(progn
                 (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
                 (funcall (intern "QUICKLOAD" "QL") :qlot :silent t)
                 (progv (list (find-symbol "*PROJECT-ROOT*" "QLOT"))
                     (list ,(uiop:native-namestring tools))
                   ,(if projects
                        `(funcall (find-symbol "UPDATE" "QLOT") ',projects)
                        `(funcall (find-symbol "INSTALL" "QLOT")))))))
    (zerop (nth-value 2 (uiop:run-program (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                                                "--noinform" "--non-interactive" "--no-userinit"
                                                "--eval" (with-standard-io-syntax
                                                           (prin1-to-string form)))
                                          :directory           tools
                                          :output              ':interactive
                                          :error-output        ':interactive
                                          :ignore-error-status t)))))

(defun check--tools-complete-p (tools)
  "Return T when every Git tool the qlfile of TOOLS names has an installed release.

qlot can report success after a network failure left a dist out."
  (every (lambda (line)
           (let ((words (uiop:split-string line :separator " ")))
             (or (not (equal (first words) "git"))
                 (and (uiop:subdirectories
                       (merge-pathnames (format nil ".qlot/dists/~A/software/" (second words)) tools))
                      t))))
         (uiop:read-file-lines (merge-pathnames "qlfile" tools))))

(defun check--touch (file)
  "Create or rewrite FILE, so its write date records the current time."
  (with-open-file (stream file :direction ':output :if-exists ':supersede)
    (write-line (file-namestring file) stream)))

(defun check--ensure-tools (root)
  "Install ROOT's development tools unless installed, refresh floating ones when due, and return their setup file."
  (let* ((tools     (check--tool-root root))
         (installed (merge-pathnames "installed" tools))
         (refreshed (merge-pathnames "refreshed" tools)))
    (unless (probe-file installed)
      (format t "~&Installing the locked development tools.~%")
      (finish-output)
      (dolist (file (check--tool-files root))
        (uiop:copy-file file (ensure-directories-exist (merge-pathnames (file-namestring file) tools))))
      (unless (and (check--run-qlot tools) (check--tools-complete-p tools))
        (format t "~&Installing the development tools failed; the next check starts over.~%")
        (uiop:delete-directory-tree tools :validate t)
        (uiop:quit 2))
      (check--touch installed))
    (when (or (not (probe-file refreshed))
              (> (- (get-universal-time) (file-write-date refreshed)) *check-refresh-seconds*))
      (format t "~&Refreshing ~{~A~^, ~}.~%" *check-floating-tools*)
      (finish-output)
      (if (apply #'check--run-qlot tools *check-floating-tools*)
          (check--touch refreshed)
          (format t "~&Refreshing the development tools failed; using the installed versions.~%")))
    (merge-pathnames ".qlot/setup.lisp" tools)))

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname (truename *load-truename*)))))
  (load (check--ensure-tools root))
  (asdf:load-system "cheque")
  (uiop:quit (uiop:symbol-call '#:cheque '#:main (uiop:command-line-arguments) :root root)))
