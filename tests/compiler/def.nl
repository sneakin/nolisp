;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def f (x)
  (if x 12 34))

(values (f 123) (f 0))
(asm (halt))
