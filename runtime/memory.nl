;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bitops")
(require "type-sizes")
(require "runtime/math")

(defun ptr-read-long (ptr)
  (asm (load 0 0 11) 4
       (load 0 0 0) 0))

(defun ptr-read-ulong (ptr)
  (ptr-read-long ptr))

(defun ptr-read-byte (ptr)
  (logand (ptr-read-long ptr) #xFF))

(defun ptr-write-long (c ptr)
  (asm (load 1 0 11) 8
       (load 0 0 11) 4
       (store 1 0 0) 0
       (inc 0) 4))

(defun ptr-write-ulong (c ptr)
  (ptr-write-long c ptr))

(defun ptr-write-byte (c ptr)
  (ptr-write-long (logior (ptr-read-long ptr) (logand c #xFF)) ptr))

(defun ptr-read-float (ptr)
  (ptr-read-long ptr))

(defun ptr-write-float (n ptr)
  (ptr-write-long n ptr))

(defun ptr-copy (src dest count)
  (if (> count *SIZEOF_LONG*)
      (progn
        (ptr-write-long (ptr-read-ulong src) dest)
        (ptr-copy (+ src *SIZEOF_LONG*) (+ dest *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
      (if (> count 0)
          (progn
            (ptr-write-byte (ptr-read-byte src) dest)
            (ptr-copy (+ src 1) (+ dest 1) (- count 1)))
          dest)))
