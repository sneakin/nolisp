;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def f (x y)
  (let ((g (lambda (a b) (values a b))))
    (g x y)))

(f 123 456)
(asm (halt))
