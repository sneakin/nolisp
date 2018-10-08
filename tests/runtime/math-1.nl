;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/math.nl")

(values (+ 2 2)
        (- 8 2)
        (* 2 4)
        (/ 8 2)
        (* (+ 9 (+ 8 7))
           (- 10 (/ 5 3))))

(asm (halt))
