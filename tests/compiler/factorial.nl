;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Computes the factorial of 5 in about 284 cycles and 20 at 1304 cycles,
;;; about 1024 more cycles

(def - (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls)
       (subi 1 0 14)))

(def * (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (muli 1)))

(def fact (n acc)
     (if n
         (fact (- n 1) (* n acc))
       acc))

(fact 5 1)
(asm (halt))
(fact 20 1)
(asm (halt))
