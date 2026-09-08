;;;; -- Fresh-image test loader --

(require :asdf)
(push (uiop:pathname-directory-pathname (truename "clinedi.asd"))
      asdf:*central-registry*)
(asdf:load-asd (truename "clinedi.asd"))
(asdf:test-system "clinedi")
