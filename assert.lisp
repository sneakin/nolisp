;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl (in-package :repl)

#+:repl (require "runtime/eq")
#+:repl (require "runtime/logic")
#+:repl (require "runtime/error")
#+:repl (require "runtime/halt")

#+:repl
(defun assert (v)
  (if (not v)
      (error 'assertion-failed)))

(defun assert-eq (a b)
  (assert (eq a b)))

(defun assert-not-eq (a b)
  (assert (not (eq a b))))

(defun assert-equal (a b)
  (assert (equal a b)))

(defun assert-not-equal (a b)
  (assert (not (equal a b))))

#+:repl
(defun run-test-suite (suite)
  (suite)
  (values 411 900)
  (halt))
