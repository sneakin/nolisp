;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun float (n)
  (asm (load 0 0 11) 4
       (convi 0 0 4)))

(defun ceiling (n)
  (asm (load 0 0 11) 4
       (ceilf 0 0)))

(defun floorf (n)
  (asm (load 0 0 11) 4
       (floorf 0 0)))
