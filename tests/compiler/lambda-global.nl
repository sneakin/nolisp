;;; -*- mode: Lisp; coding: utf-8-unix -*-

(var f (lambda fn (a b) (values a b)))
(f 123 456)
(asm (halt))
