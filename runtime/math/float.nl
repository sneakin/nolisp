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
       (addf 1 14)
       (load 1 0 11) 4
       (cls 7)
       (addf 1 14)))

(def --float (a)
  (asm (load 0 0 15) 0.0
       (load 1 0 11) 4
       (cls 7)
       (subf 1 14)))

(def --float (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (subf 1 14)))

(def --float (a b c)
  (asm (load 0 0 11) 12
       (load 1 0 11) 8
       (cls 7)
       (subf 1 14)
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

(def abs-float (n)
  (if (<-float n 0.0)
      (--float n)
      n))

(defun ceiling (n)
  (asm (load 0 0 11) 4
       (ceilf 0 0)))

(defun floorf (n)
  (asm (load 0 0 11) 4
       (floorf 0 0)))

(defun roundf (n)
  (asm (load 0 0 11) 4
       (roundf 0 0)))
