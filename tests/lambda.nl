;;; -*- mode: Lisp; coding: utf-8-unix -*-

(var g (lambda (x) (if x 123 456)))
(var h (lambda (x) (if x -123 -456)))
(def f (fn x) (fn x))

(values (f g 1) (f h 1))
(asm (halt))