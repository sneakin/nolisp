;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/truth")
(require "runtime/eq")

(defun numberp (obj)
  t)

(defun = (a b)
  (eq a b))
