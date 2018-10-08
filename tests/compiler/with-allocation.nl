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

(with-allocation (a 14)
  (with-allocation (b 64)
    (with-allocation (c 31)
      (seq-write a 11 456)
      (seq-write a 0 123)
      (seq-write b 0 789)
      (seq-write c 0 101)
      ;; zero registers
      (values 0 0 0 0 0 0 0)
      (values a b c
              (seq-read a 0)
              (seq-read a 11)
              (seq-read b 0)
              (seq-read c 0)))))

(asm (halt))
