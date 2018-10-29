;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/logic")

(defun test-not ()
  (assert-equal (not nil) t)
  (assert-equal (not t) nil)
  (assert-equal (not 123) nil)
  (assert-equal (not 'symbol) nil)
  (assert-equal (not "string") nil))

(defun test-and ()
  (assert-equal (and nil nil) nil)
  (assert-equal (and nil t) nil)
  (assert-equal (and t nil) nil)
  (assert-equal (and t t) t)
  (assert-equal (and 1 2) 2)
  (assert-equal (and 2 1) 1))

(defun test-or ()
  (assert-equal (or nil nil) nil)
  (assert-equal (or nil t) t)
  (assert-equal (or t nil) t)
  (assert-equal (or t t) t)
  (assert-equal (or 1 2) 1)
  (assert-equal (or nil 1) 1))

(defun test-logic ()
  (test-not)
  (test-and)
  (test-or))

(run-test-suite test-logic/0)
