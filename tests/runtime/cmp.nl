;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/cmp.nl")
(require "runtime/halt.nl")

(defun test-lt ()
  (assert-equal (< 10 3) 0)
  (assert-equal (< 3 10) 1)
  (assert-equal (< 10 10) 0)
  (assert-equal (< -10 10) 1)
  (assert-equal (< 10 -10) 0))

(defun test-lte ()
  (assert-equal (< 10 3) 0)
  (assert-equal (< 3 10) 1)
  (assert-equal (<= 10 10) 1)
  (assert-equal (<= -10 -10) 1)
  (assert-equal (<= -10 10) 1)
  (assert-equal (<= 10 -10) 0))

(defun test-gt ()
  (assert-equal (> 10 3) 1)
  (assert-equal (> 3 10) 0)
  (assert-equal (> 10 10) 0)
  (assert-equal (> -10 10) 0)
  (assert-equal (> 10 -10) 1)
  (assert-equal (> -10 -10) 0))

(defun test-gte ()
  (assert-equal (>= 10 3) 1)
  (assert-equal (>= 3 10) 0)
  (assert-equal (>= 10 10) 1)
  (assert-equal (>= -10 10) 0)
  (assert-equal (>= 10 -10) 1)
  (assert-equal (>= -10 -10) 1))

(defun test-cmp ()
  (test-lt)
  (test-lte)
  (test-gt)
  (test-gte))

(run-test-suite test-cmp)
