;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/truth")

(defun not (a)
  (if a nil t))

(defun or (a b)
  (if a a b))

(defun or (a b &optional c d e)
  (cond
    (a t)
    (b t)
    (c t)
    (d t)
    (e t)
    (t nil)))

(defun and (a b)
  (if a
      (if b b nil)
      nil))

(defun and (a b c)
  (if a
      (if b (if c c nil)
          nil)
      nil))
