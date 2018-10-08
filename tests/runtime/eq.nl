;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/eq")

(defun test-eq ()
  (assert-equal (eq 0 0) 1)
  (assert-equal (eq 2 4) 0)
  (assert-equal (eq 2 2) 1)
  (assert-equal (eq -10 -10) 1)
  (assert-equal (eq 'hello 'world) 0)
  (assert-equal (eq 'hello 'hello) 1))

(defun test-suite-eq ()
  (test-eq)
  (values 411 900))

(test-suite-eq)
(halt)
