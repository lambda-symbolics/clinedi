;;;; -- Fresh-image test loader --

(require :asdf)
(let ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  ;; Third-party dependencies such as trivial-gray-streams come from Quicklisp.
  (when (probe-file quicklisp)
    (load quicklisp)))
(push (uiop:pathname-directory-pathname (truename "clinedi.asd"))
      asdf:*central-registry*)
(asdf:load-asd (truename "clinedi.asd"))
(asdf:test-system "clinedi")
