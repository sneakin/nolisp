;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def + (a b)
  (asm (mov 0 1)
       (cls 7)
       (addi 2 14)))

(def - (a b)
  (asm (mov 0 1)
       (cls 7)
       (subi 2 14)))

(def * (a b)
     (asm (mov 0 1)
          (muli 2)))

(def / (a b)
     (asm (mov 0 1)
          (divi 2)))

(def expt (base power)
  (asm (mov 0 1)
       (powi 2)))
