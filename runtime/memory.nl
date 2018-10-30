;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bitops")
(require "type-sizes")
(require "runtime/math")

(defun ptr-read-long (ptr)
  (asm (load 0 0 11) 4
       (load 0 0 0) 0))

(defun ptr-read-ulong (ptr)
  (ptr-read-long ptr))

(defun ptr-read-ubyte (ptr)
  (logand (ptr-read-long ptr) #xFF))

(defun ptr-read-byte (ptr)
  ;; this let triggering a bug in memory-1.nl
  (let ((v (ptr-read-ubyte ptr)))
    (if (> v 127)
        (- v 256)
        v)))

;; (defun ptr-read-byte (ptr)
;;   (ptr-read-ubyte ptr))

(defun ptr-write-long (c ptr)
  (asm (load 1 0 11) 8
       (load 0 0 11) 4
       (store 1 0 0) 0
       (inc 0) 4))

(defun ptr-write-ulong (c ptr)
  (ptr-write-long c ptr))

(defun ptr-write-ubyte (c ptr)
  (ptr-write-long (logior (logand (ptr-read-long ptr) #xFFFFFF00)
                          (logand c #xFF))
                  ptr)
  (+ ptr 1))

(defun ptr-write-byte (c ptr)
  (ptr-write-ubyte c ptr))

(defun ptr-write-char (c ptr)
  (ptr-write-ubyte c ptr))

(defun ptr-read-float (ptr)
  (ptr-read-long ptr))

(defun ptr-write-float (n ptr)
  (ptr-write-long n ptr))

(defun ptr-cmp-bytes (a b count)
  (if (> count 0)
      (let* ((ac (ptr-read-ubyte a))
             (bc (ptr-read-ubyte b)))
        (if (eq ac bc)
            (ptr-cmp-bytes (+ a 1) (+ b 1) (- count 1))
            nil))
      t))

(defun ptr-cmp-ulong (a b count)
  (if (>= count *SIZEOF_LONG*)
      (let* ((ac (ptr-read-ulong a))
             (bc (ptr-read-ulong b)))
        (if (eq ac bc)
            (ptr-cmp-ulong (+ a *SIZEOF_LONG*) (+ b *SIZEOF_LONG*) (- count *SIZEOF_LONG*))
            nil))
      (ptr-cmp-bytes a b count)))

(defun ptr-cmp (a b count)
  (if (eq a b)
      t
      (ptr-cmp-ulong a b count)))

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
