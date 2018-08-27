;;; -*- mode: Lisp; coding: utf-8-unix -*-
(defun a (x)
  (if x 123))

(defun b (x)
  (if x))

(defun c (x)
  (if x 123 456))

(values (a 0) (a 1) (b 0) (b 2) (c 0) (c 1))
;;; => 0 123 0 0 456 123
