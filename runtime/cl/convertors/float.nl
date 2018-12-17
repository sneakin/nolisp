;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(defun make-float-decimal (n f &optional (negate (< n 0)) (base 10.0) (digits (ceiling (log f base))))
  (let ((f (if (> f 0.0)
               (/ f (expt base digits))
               0.0)))
    (if negate
        (- n f)
        (+ n f))))
