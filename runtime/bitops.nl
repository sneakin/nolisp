;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/eq")
(require "runtime/cmp")

(defun ash (n bit)
  (if (eq n 0)
      n
      (if (> n 0)
          (asm (mov 0 1)
               (bsl 1))
          (asm (mov 0 1)
               (bsr 1)))))

(defun logand (n mask)
  (asm (mov 0 1)
       (and 1)))

(defun logior (n mask)
  (asm (mov 0 1)
       (or 1)))
