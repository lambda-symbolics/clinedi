(in-package #:clinedi/tests)

(defun run-paste-tests ()
  "Test reading one pasted path from shell quoting and file URLs."
  (dolist (case '(("/tmp/plain.png" "/tmp/plain.png")
                  ("  '/tmp/with space.png'  " "/tmp/with space.png")
                  ("\"/tmp/double quoted.png\"" "/tmp/double quoted.png")
                  ("/tmp/escaped\\ space.png" "/tmp/escaped space.png")
                  ("file:///tmp/url%20space.png" "/tmp/url space.png")
                  ("file:///tmp/%C5%BElu%C5%A5ou%C4%8Dk%C3%BD.png" "/tmp/žluťoučký.png")
                  ("/tmp/one /tmp/two" nil)
                  ("'/tmp/unterminated" nil)
                  ("/tmp/trailing\\" nil)
                  ("file:///tmp/bad%2" nil)
                  ("file:///tmp/bad%FF" nil)
                  ("" nil)))
    (destructuring-bind (text expected) case
      (check-equal (format nil "pasted ~S reads as ~S" text expected)
                   expected (clinedi:pasted-path text))))
  (check-equal "a quoted word keeps its inner spaces" "a b"
               (clinedi:pasted-path-token "'a b'")))
