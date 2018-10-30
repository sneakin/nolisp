;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions

(require "runtime/bitops")

(def + (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (addi 1 14)))

(def - (a)
  (asm (load 0 0 11) 4
       (neg 0)))

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

(def mod (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (modi 1)))

(def expt (base power)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (powi 1)))

(def logi2 (n &optional (count 0))
  (let ((next (ash n -1)))
    (if (> next 0)
        (logi2 next (+ count 1))
        count)))

(def logi (n base)
  (if (eq base 2)
      (logi2 n)
      (/ (logi2 n) (logi2 base))))

(def floori (n)
  n)
