(asdf:defsystem "colorlisp"
  :description "Style-neutral syntax highlighting for Common Lisp applications"
  :author "ColorLisp contributors"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("babel"
               "cffi"
               "cl-ppcre")
  :serial t
  :components ((:file "src/package")
               (:file "src/conditions")
               (:file "src/native-build")
               (:file "src/ffi")
               (:file "src/languages")
               (:file "src/highlight"))
  :in-order-to ((test-op (test-op "colorlisp/tests"))))

(asdf:defsystem "colorlisp/tests"
  :description "Tests for ColorLisp"
  :depends-on ("colorlisp")
  :serial t
  :components ((:file "tests/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:colorlisp/tests '#:run-tests)
               (error "ColorLisp tests failed."))))
