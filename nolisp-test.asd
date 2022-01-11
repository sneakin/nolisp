(defsystem "nolisp-test"
  :description "nolisp-test: Tests for Nolan's Lisp"
  :version "3.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  :depends-on ("nolisp")
  :components (
    (:file "src/testing/nassert")
    (:file "src/testing/assert-match" :depends-on ("src/testing/nassert"))
    (:file "tests/match-test.lisp" :depends-on ("src/testing/nassert"))
    (:file "tests/range-test.lisp" :depends-on ("src/testing/nassert"))
 )
)
