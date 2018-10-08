;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun seq-read  (ptr offset)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls)
       (addi 1 14)
       ;;(inc 0) 4
       (load 0 0 0) 0))

(defun seq-write  (ptr offset value)
  (asm (load 0 0 11) 12 ; ptr => R0
       (load 1 0 11) 4 ; value => R1
       (load 2 0 11) 8 ; offset => R2
       (cls)
       (addi 2 14) ; ptr + offset
       (store 1 0 0) 0
       ;; return (values (+ ptr offset) value)
       (inc 0) 4))

(with-allocation (b 64)
  (with-allocation (a 64)
    (seq-write a 60 456)
    (seq-write a 0 123)
    ;; zero registers
    (values 0 0 0 0 0 0 0)
    (values a
            (seq-read a 0)
            (seq-read a 4)
            (seq-read a 60))))

(asm (halt))
