;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions specialized for floats.

(defun float (n)
  (asm (load 0 0 11) 4
       (convi 0 4)))
