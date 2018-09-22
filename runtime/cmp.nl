;;; -*- mode: Lisp; coding: utf-8-unix -*-

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
