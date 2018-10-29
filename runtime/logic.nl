;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/truth")

(defun not (a)
  (if a nil t))

(defun or (a b)
  (if a a b))

(defun and (a b)
  (if a
      (if b b nil)
      nil))
