;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Computes the factorial of 5 in about 279 cycles and 20 in 999 more cycles

(def - (a b)
     (asm (mov 0 1)
          (cls)
          (subi 2 0 14)))

(def * (a b)
     (asm (mov 0 1)
          (muli 2)))

(def fact (n acc)
     (if n
         (fact (- n 1) (* n acc))
       acc))

(fact 5 1)
(asm (halt))
(fact 20 1)
(asm (halt))
