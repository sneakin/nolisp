;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun ptr-read-long (ptr)
  (asm (load 1 0 0)
       0))

(defun ptr-read-byte (ptr)
  (logand (ptr-read-long ptr) #xFF))

(defun ptr-write-long (c ptr)
  (asm (store 1 0 2)
       0))

(defun ptr-write-byte (c ptr)
  (ptr-write-long (logior (ptr-read-long ptr) (logand c #xFF)) ptr))
