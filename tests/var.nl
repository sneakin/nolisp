;;; -*- mode: Lisp; coding: utf-8-unix -*-

(var x 123)
(var y 456)

(set y 789)

(def e () 0)
(def f () -123)

(values x y f (f))
(asm (halt))
