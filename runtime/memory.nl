;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun ptr-read-byte (ptr)
  (asm (mov 0 1)
       (cls)
       (addi 2 0 14)
       (load 0 0 0)
       0))

(defun ptr-write-byte (c ptr)
  (asm (store 1 0 0)
       0))

