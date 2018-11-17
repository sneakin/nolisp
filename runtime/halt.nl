;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun halt ()
  (asm (halt)))

(defun sleep ()
  (asm (sleep)))

(defun clear-status-bit (mask)
  (asm (load 1 0 11) 4
       (not 1 1)
       (mov 0 14)
       (and 1)
       (mov 14 0)))

(defun wakeup ()
  (clear-status-bit #x20))
