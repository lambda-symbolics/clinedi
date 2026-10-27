(require :asdf)

(let* ((script-path      (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root      (uiop:pathname-parent-directory-pathname script-directory))
       (quicklisp-setup  (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
  (unless (probe-file quicklisp-setup)
    (error "clinedi bootstrap needs Quicklisp at ~A"
           quicklisp-setup))
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload :qlot :silent t)
  (let ((qlot-project-root (find-symbol "*PROJECT-ROOT*" "QLOT")))
    (unless qlot-project-root
      (error "The loaded Qlot does not expose its project root."))
    (progv (list qlot-project-root) (list source-root)
      (uiop:with-current-directory (source-root)
        (uiop:symbol-call '#:qlot '#:install)))))
