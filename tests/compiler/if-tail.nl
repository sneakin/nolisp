;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")

(defun fn (x) x)

(defun test-if-tail ()
  (if t 12 (error 'wrong-branch))
  (if t (fn 123) (error 'wrong-branch))
  (if 0 (error 'wrong-branch) 456)
  (assert-equal (if t 12 23) 12)
  (assert-equal (if 0 12 23) 23)
  (assert-equal (if t (values 12 23) (values 34 43)) 12)
)

(run-test-suite test-if-tail)
