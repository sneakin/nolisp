;;; -*- mode: Lisp; coding: utf-8-unix -*-

(var x 123)
(var y 456)

(set y 789)
(values x y)
(asm (halt))
