;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions specialized for floats.

(in-package :repl)

(defun +-float (a b &optional (c 0.0))
  (+ a b c))

(defun --float (a b &optional (c 0.0))
  (- a b c))

(defun *-float (a b)
  (* a b))

(defun /-float (a b)
  (/ a b))

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

(defun make-float-2i (n f)
  (+ n (if (> f 0.0)
           (/ f (expt 10.0 (ceiling (log f 10.0))))
           0.0)))
