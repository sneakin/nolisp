;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/eq")
(require "runtime/cmp")

(def - (a)
  (asm (load 0 0 11) 4
       (neg 0)))

(defun bsl (n bit)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls)
       (bsl 1 15)))

(defun bsr (n bit)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls)
       (bsr 1 15)))

(defun ash (n bit)
  (if (eq bit 0)
      n
      (if (> bit 0)
          (bsl n bit)
          (bsr n (- bit)))))

(defun logand (n mask)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (and 1)))

(defun logior (n mask)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (or 1)))

(defun logior (a b c d)
  (asm (load 0 0 11) 16
       (load 1 0 11) 12
       (or 1)
       (load 1 0 11) 8
       (or 1)
       (load 1 0 11) 4
       (or 1)))
