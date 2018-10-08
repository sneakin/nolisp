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

(defun assert-equal (a b)
  (assert (equal a b)))

#+:repl
(defun run-test-suite (suite)
  (suite)
  (values 411 900)
  (halt))
