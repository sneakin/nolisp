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

(def < (a b)
  (asm (cmp 1 2)
       (load 0 0 15)
       0
       (load 0 2 15)
       1))

(def <= (a b)
  (asm (cmp 1 2)
       (load 0 0 15)
       0
       (load 0 2 15)
       1
       (load 0 1 15)
       1))

(def > (a b)
  (asm (cmp 2 1)
       (load 0 0 15)
       0
       (load 0 4 15)
       1))

(def >= (a b)
  (asm (cmp 2 1)
       (load 0 0 15)
       1
       (load 0 2 15)
       1
       (load 0 1 15)
       0))

(def expt (base power)
  (asm (mov 0 1)
       (powi 2)))
