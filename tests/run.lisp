(require :asdf)
(require :nolisp)

(load "src/testing/nassert.lisp")
(load "src/testing/assert-match.lisp")
(load "tests/units.lisp")

(test-match)
(test-range)
(test-nc-macroexpand-1)
(test-nc-macroexpand)
(test-lookup-walker)
(test-cps-transform)
(test-nc-compile)
