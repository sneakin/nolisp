;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun float (n)
  (asm (convi 1 0 4)))

(defun ceiling (n)
  (asm (ceilf 1 0)))
