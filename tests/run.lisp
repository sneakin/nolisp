(require :asdf)
(require :nolisp)

(use-package :nolisp)

(load "src/testing/nassert.lisp")
(load "src/testing/assert-match.lisp")

(defun load-test-units ()
  (let ((testdir (namestring (first (directory "./")))))
    (mapcar #'(lambda (p) (print p) (load p))
            (directory "./tests/**/*-test.lisp"))))

(load-test-units)

(test-match)
(test-range)
(test-fun)
(test-list)
(test-lerp)
(test-scan-list)
(test-macro-expand-1)
(test-macro-expand)
(test-lookup-walker)
(test-cps-transform)
(test-nc-compile)
