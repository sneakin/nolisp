;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bitops")

(defun ptr-read-long (ptr)
  (asm (load 1 0 0)
       0))

(defun ptr-read-ulong (ptr)
  (ptr-read-long ptr))

(defun ptr-read-byte (ptr)
  (logand (ptr-read-long ptr) #xFF))

(defun ptr-write-long (c ptr)
  (asm (store 1 0 2)
       0))

(defun ptr-write-ulong (c ptr)
  (ptr-write-long c ptr))

(defun ptr-write-byte (c ptr)
  (ptr-write-long (logior (ptr-read-long ptr) (logand c #xFF)) ptr))

(defun ptr-read-float (ptr)
  (ptr-read-long ptr))

(defun ptr-write-float (n ptr)
  (ptr-write-long n ptr))
