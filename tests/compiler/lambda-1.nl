;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def h1 ()
  (lambda fn (a b) (values a b)))

((h1) 123 456)
(asm (halt))
