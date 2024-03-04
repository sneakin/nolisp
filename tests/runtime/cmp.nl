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

(defun test-lt-unsigned ()
  (assert-equal (<-unsigned 10 3) 0)
  (assert-equal (<-unsigned 3 10) 1)
  (assert-equal (<-unsigned 10 10) 0)
  (assert-equal (<-unsigned #xF0000000 #x10) 0)
  (assert-equal (<-unsigned #x10 #xF0000000) 1)
  (assert-equal (<-unsigned #xF0000000 #xF0000000) 0))

(defun test-lte ()
  (assert-equal (< 10 3) 0)
  (assert-equal (< 3 10) 1)
  (assert-equal (<= 10 10) 1)
  (assert-equal (<= -10 -10) 1)
  (assert-equal (<= -10 10) 1)
  (assert-equal (<= 10 -10) 0))

(defun test-lte-unsigned ()
  (assert-equal (<=-unsigned 10 3) 0)
  (assert-equal (<=-unsigned 3 10) 1)
  (assert-equal (<=-unsigned 10 10) 1)
  (assert-equal (<=-unsigned #xF0000000 #x10) 0)
  (assert-equal (<=-unsigned #x10 #xF0000000) 1)
  (assert-equal (<=-unsigned #xF0000001 #xF0000001) 1))

(defun test-gt ()
  (assert-equal (> 10 3) 1)
  (assert-equal (> 3 10) 0)
  (assert-equal (> 10 10) 0)
  (assert-equal (> -10 10) 0)
  (assert-equal (> 10 -10) 1)
  (assert-equal (> -10 -10) 0))

(defun test-gt-unsigned ()
  (assert-equal (>-unsigned 10 3) 1)
  (assert-equal (>-unsigned 3 10) 0)
  (assert-equal (>-unsigned 10 10) 0)
  (assert-equal (>-unsigned #xF0000000 #x10) 1)
  (assert-equal (>-unsigned #x10 #xF0000000) 0)
  (assert-equal (>-unsigned #xF0000000 #xF0000000) 0))

(defun test-gte ()
  (assert-equal (>= 10 3) 1)
  (assert-equal (>= 3 10) 0)
  (assert-equal (>= 10 10) 1)
  (assert-equal (>= -10 10) 0)
  (assert-equal (>= 10 -10) 1)
  (assert-equal (>= -10 -10) 1))

(defun test-gte-unsigned ()
  (assert-equal (>=-unsigned 10 3) 1)
  (assert-equal (>=-unsigned 3 10) 0)
  (assert-equal (>=-unsigned 10 10) 1)
  (assert-equal (>=-unsigned #xF0000000 #x10) 1)
  (assert-equal (>=-unsigned #x10 #xF0000000) 0)
  (assert-equal (>=-unsigned #xF0000001 #xF0000001) 1))

(defun test-cmp ()
  (test-lt)
  (test-lt-unsigned)
  (test-lte)
  (test-lte-unsigned)
  (test-gt)
  (test-gt-unsigned)
  (test-gte)
  (test-gte-unsigned))

(run-test-suite test-cmp)