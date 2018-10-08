;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/logic")

(defun test-not ()
  (assert-equal (not 0) t)
  (assert-equal (not nil) t)
  (assert-equal (not 0.0) t)
  (assert-equal (not 1) nil)
  (assert-equal (not 1.0) nil)
  (assert-equal (not 'symbol) nil))

(run-test-suite test-not)
