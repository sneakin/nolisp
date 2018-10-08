;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/math.nl")
(require "runtime/halt.nl")

(defun test-add ()
  (assert-equal (+ 2 2) 4)
  (assert-equal (+ -2 2) 0))

(defun test-sub ()
  (assert-equal (- 2 2) 0)
  (assert-equal (- -2 2) -4)
  (assert-equal (- 4 2) 2))

(defun test-mul ()
  (assert-equal (* 2 2) 4)
  (assert-equal (* -2 2) -4)
  (assert-equal (* 2 1) 2)
  (assert-equal (* 0 2) 0))

(defun test-div ()
  (assert-equal (/ 2 2) 1)
  (assert-equal (/ -2 2) -1)
  (assert-equal (/ 4 2) 2)
  (assert-equal (/ 2 4) 0))

(defun test-expt ()
  (assert-equal (expt 2 3) 8)
  (assert-equal (expt 8 -1) 0)
  (assert-equal (expt 100 1) 100)
  (assert-equal (expt 100 0) 1))

(defun test-math ()
  (test-add)
  (test-sub)
  (test-mul)
  (test-div)
  (test-expt))

(run-test-suite test-math)
