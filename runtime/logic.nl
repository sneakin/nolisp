;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun or (a b)
  (if a a b))

(defun and (a b)
  (if a
      (if b b nil)
      nil))