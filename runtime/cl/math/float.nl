;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions specialized for floats.

(in-package :repl)

(defun +-float (&rest args)
  (apply #'+ args))

(defun --float (&rest args)
  (apply #'- args))

(defun *-float (&rest args)
  (apply #'* args))

(defun /-float (&rest args)
  (apply #'/ args))

(defun mod-float (a b)
  (mod a b))

(defun expt-float (base power)
  (expt base power))

(defun log2-float (n)
  (log2 n))

(defun log-float (n base)
  (log n base))

(defun floorf (n)
  (floor n))
