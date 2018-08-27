;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Should raise an function-arity-error

(defun a (x &optional y)
  (values x y))

(a/2 1)
