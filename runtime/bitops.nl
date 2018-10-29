;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/eq")
(require "runtime/cmp")

(defun ash (n bit)
  (if (eq bit 0)
      n
      (if (> bit 0)
          (asm (load 0 0 11) 8
               (load 1 0 11) 4
               (cls)
               (bsl 1 15))
          (asm (load 1 0 11) 4
               (neg 1)
               (mov 1 0)
               (load 0 0 11) 8
               (cls)
               (bsr 1 15)))))

(defun logand (n mask)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (and 1)))

(defun logior (n mask)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (or 1)))
