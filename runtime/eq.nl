;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/truth")

(defun eq (a b)
  (asm (cmp 1 2)
       (load 0 1 15) 0))
