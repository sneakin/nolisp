;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun error (kind)
  (asm (load 0 0 15) 911
       (load 1 0 11) 4
       (mov 2 11) ;; sp
       (mov 3 12) ;; ip
       (halt)))
