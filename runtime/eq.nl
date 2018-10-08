;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/truth")

(defun eq (a b)
  (asm (load 0 0 11) 4
       (load 1 0 11) 8
       (cmp 0 1)
       (load 0 0 15) 0
       (load 0 1 15) 1))

(defun equal (a b)
  (eq a b))
