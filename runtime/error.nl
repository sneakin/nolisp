;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun console-write (str &optional (n 0)))
(defun console-write-unsigned-integer (n &optional (base 10)))

(defun error (kind &optional a b c d e f g)
  (console-write "ERROR:")
  (console-write kind)
  (console-write b 16)
  (console-write d 16)
  (console-write f 16)
  (console-write-unsigned-integer a 16)
  (console-write-unsigned-integer b 16)
  (console-write-unsigned-integer c 16)
  (console-write-unsigned-integer d 16)
  (console-write-unsigned-integer e 16)
  (console-write-unsigned-integer f 16)
  (console-write-unsigned-integer g 16)
  (asm (push 0)
       (push 11) ;; sp
       (push 12) ;; ip
       (load 0 0 15) 911
       ;; (load 1 0 11) 32 ;; kind
       ;; (load 4 0 11) 28 ;; a
       ;; (load 5 0 11) 24 ;; b
       ;; (load 6 0 11) 20 ;; c
       ;; (load 7 0 11) 16 ;; d
       ;; (load 8 0 11) 12 ;; e
       ;; (load 9 0 11) 8 ;; f
       ;; (load 10 0 11) 4 ;; g
       (halt)))
