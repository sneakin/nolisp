;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Arithmetic functions specialized for unsigned integers.

(require "runtime/bitops")

(def /-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (divu 1)))

(def mod-unsigned (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (modu 1)))

(def expt-unsigned (base power)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (powu 1)))

(def log2-unsigned (n)
  (asm (load 0 0 11) 4
       (logu 0)))

#+:never
(def log2-unsigned (n &optional (count 0))
  (let ((next (ash n -1)))
    (if (>-unsigned next 0)
        (log2-unsigned next (+ count 1))
        (+ 1 count))))

(def log-unsigned (n base)
  (if (eq base 2)
      (log2-unsigned n)
      (/ (log2-unsigned n) (log2-unsigned base))))
