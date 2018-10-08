;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def g (a b) (values a b))

(def f (x y)
  (g x y))

(f 123 456)
(asm (halt))
