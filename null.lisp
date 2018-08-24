;;; -*- mode: Lisp; coding: utf-8-unix -*-
#+:sbcl (in-package :repl)

#+:repl (require "runtime/eq.nl")

(defun null? (c)
  (eq c 0))
