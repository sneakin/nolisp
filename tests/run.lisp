(require :asdf)
(require :nolisp)

(load "src/testing/nassert.lisp")
(load "src/testing/assert-match.lisp")
(load "tests/units.lisp")

(test-match)
(test-range)
(test-macro-expand-1)
(test-macro-expand)
(test-lookup-walker)
(test-cps-transform)
(test-nc-compile)
