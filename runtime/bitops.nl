;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/eq")
(require "runtime/cmp")

(defun ash (n bit)
  (if (eq n 0)
      n
      (if (> n 0)
          (asm (load 0 0 11) 8
               (load 1 0 11) 4
               (bsl 1))
          (asm (load 0 0 11) 8
               (load 1 0 11) 4
               (neg 1)
               (bsr 1)))))

(defun logand (n mask)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (and 1)))

(defun logior (n mask)
  (asm (load 0 0 11) 8
       (load 0 0 11) 4
       (or 1)))
