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
                 (:file "editor-state")
                 (:file "selector")
                 (:file "session")
                 (:file "input")
                 (:file "transport")
                 (:file "render")
                 (:file "live-region")
                 (:file "terminal-editor"))))
  :in-order-to ((asdf:test-op (asdf:test-op "clinedi/tests"))))

(asdf:defsystem "clinedi/tests"
  :description "Regression tests for Clinedi"
  :encoding :utf-8
  :depends-on ("clinedi"
               #+(and sbcl (not win32)) "clinedi/posix"
               #+(and sbcl win32) "clinedi/win32")
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "unicode")
                 (:file "editor")
                 (:file "editor-state")
                 (:file "selector")
                 (:file "session")
                 (:file "input")
                 (:file "transport")
                 (:file "render")
                 (:file "live-region")
                 (:file "terminal-editor")
                 (:file "check"))))
  :perform (asdf:test-op
            (operation system)
            (declare (ignore operation system))
            (uiop:symbol-call '#:clinedi/tests '#:run-tests)))

(asdf:defsystem "clinedi/posix"
  :description "Optional SBCL POSIX terminal input-mode and geometry adapter"
  :depends-on ("clinedi" "sb-posix")
  :components ((:file "source/posix")))

(asdf:defsystem "clinedi/win32"
  :description "Optional SBCL Windows console input-mode and geometry adapter"
  :depends-on ("clinedi")
  :components ((:file "source/win32")))
