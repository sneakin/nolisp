;;; Loads nolisp and all the test files and then executes the test functions.

(require :asdf)
(require :nolisp)

(use-package :nolisp)

(load "src/testing/nassert.lisp")
(load "src/testing/assert-match.lisp")

(defun load-test-units ()
  (mapcar #'load (directory "./tests/**/*-test.lisp")))

(load-test-units)

(defun reload! ()
  (asdf:load-system :nolisp)
  (load "tests/run.lisp")
  t)

;;; Execute the test functions.
(defun run-tests ()
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
  (test-compile)
  t)
