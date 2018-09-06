;;; -*- mode: Lisp; coding: utf-8-unix -*-
#+:repl
(require "runtime/logic")

#+:sbcl (in-package :repl)

(defun null? (c)
  (eq c 0))

(defun null (c)
  (eq c 0))
