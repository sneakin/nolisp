;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def d (&optional (x 456.5))
  x)

(def c (w x)
  x)

(def b (x y) (values x y))

(def a (w &optional x (y 123) (z (c x x)))
  (values w x y z (d)))

(a 100 200)
(asm (halt))
