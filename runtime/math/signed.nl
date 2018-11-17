;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions, mostly for signed integers.

(require "runtime/bitops")

(def + (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (addi 1 14)))

(def + (a b c)
  (asm (load 0 0 11) 12
       (load 1 0 11) 8
       (cls 7)
       (addi 1 14)
       (load 1 0 11) 4
       (cls 7)
       (addi 1 14)))

(def - (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (subi 1 14)))

(def - (a b c)
  (asm (load 0 0 11) 12
       (load 1 0 11) 8
       (cls 7)
       (subi 1 14)
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

(def log2 (n)
  (asm (load 0 0 11) 4
       (logi 0)))

#+:never
(def log2 (n &optional (count 0))
  (let ((next (ash n -1)))
    (if (> next 0)
        (log2 next (+ count 1))
        (+ 1 count))))

(def log (n base)
  (if (eq base 2)
      (log2 n)
      (/ (log2 n) (log2 base))))

(def floori (n)
  n)
