;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions

(def + (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (addi 1 14)))

(def - (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (subi 1 14)))

(def * (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (muli 1)))

(def / (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (divi 1)))

(def expt (base power)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (powi 1)))
