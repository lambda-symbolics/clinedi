(asdf:defsystem "clinedi"
  :version "0.1.0"
  :author "Lukáš Hozda"
  :license "ISC"
  :description "A portable, Unicode-aware terminal line editor"
  :encoding :utf-8
  :depends-on ("cl-colorist")
  :components ((:module "source"
                :serial t
                :components
                ((:file "package")
                 (:file "unicode")
                 (:file "ansi")
                 (:file "keymap")
                 (:file "editor")
                 (:file "selector")
                 (:file "input")
                 (:file "render")
                 (:file "live-region")
                 (:file "terminal-editor"))))
  :in-order-to ((asdf:test-op (asdf:test-op "clinedi/tests"))))

(asdf:defsystem "clinedi/tests"
  :description "Regression tests for Clinedi"
  :encoding :utf-8
  :depends-on ("clinedi")
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "unicode")
                 (:file "editor")
                 (:file "selector")
                 (:file "input")
                 (:file "render")
                 (:file "live-region")
                 (:file "terminal-editor")
                 (:file "check"))))
  :perform (asdf:test-op
            (operation system)
            (declare (ignore operation system))
            (uiop:symbol-call '#:clinedi/tests '#:run-tests)))
