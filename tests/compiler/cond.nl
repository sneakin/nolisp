;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/eq")
(require "runtime/halt")

(defun f (x)
  (cond
    ((eq x 1) 100)
    ((eq x 2) 200)
    ((eq x 3) 300)
    (t 2)))

(values (f 1) (f 2) (f 3) (f 0))
(halt)
