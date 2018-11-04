;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Signed comparisons

(def < (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 0 1)
       (load 0 0 15) 0
       (load 0 2 15) 1))

(def <= (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 0 1)
       (load 0 0 15) 0
       (load 0 2 15) 1
       (load 0 1 15) 1))

(def > (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 1 0)
       (load 0 0 15) 0
       (load 0 2 15) 1))

(def >= (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 1 0)
       (load 0 0 15) 0
       (load 0 2 15) 1
       (load 0 1 15) 1))

(def <-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 0 1)
       (load 0 0 15) 0
       (load 0 4 15) 1))

(def <=-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 0 1)
       (load 0 0 15) 0
       (load 0 4 15) 1
       (load 0 1 15) 1))

(def >-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 1 0)
       (load 0 0 15) 0
       (load 0 4 15) 1))

(def >=-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cmp 1 0)
       (load 0 0 15) 0
       (load 0 4 15) 1
       (load 0 1 15) 1))
