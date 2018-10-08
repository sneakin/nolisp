;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/eq")

(defun test-mvb ()
  (multiple-value-bind (a b c)
      (values 1 2 3)
    (assert-equal a 1)
    (assert-equal b 2)
    (assert-equal c 3)))

(run-test-suite test-mvb)

