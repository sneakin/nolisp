;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions specialized for floats.

(require "runtime/bitops")
(require "runtime/float")

(def +-float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (addf 1 14)))

(def +-float (a b c)
  (asm (load 0 0 11) 12
       (load 1 0 11) 8
       (cls 7)
       (addi 1 14)
       (load 1 0 11) 4
       (cls 7)
       (addf 1 14)))

(def --float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (subf 1 14)))

(def --float (a b c)
  (asm (load 0 0 11) 12
       (load 1 0 11) 8
       (cls 7)
       (subi 1 14)
       (load 1 0 11) 4
       (cls 7)
       (subf 1 14)))

(def *-float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (mulf 1)))

(def /-float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (divf 1)))

(def mod-float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (modf 1)))

(def expt-float (base power)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (powf 1)))

(def log2-float (n)
  (asm (load 0 0 11) 4
       (logf 0)))

(def log-float (n base)
  (if (eq base 2.0)
      (log2-float n)
      (/-float (log2-float n) (log2-float base))))

(defun make-float-2i (n f)
  (+-float (float n) (if (> f 0)
                         (/-float (float f)
                                  (expt-float 10.0 (ceiling (log-float (float f) 10.0))))
                         0)))
