;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bitops")
(require "type-sizes")
(require "runtime/number")
(require "runtime/math")

(defun ptr-read-long (ptr)
  (asm (load 0 0 11) 4
       (load 0 0 0) 0))

#+:never
(defun ptr-read-ulong (ptr)
  (ptr-read-long ptr))

(defun ptr-read-ulong (ptr)
  (asm (load 0 0 11) 4
       (load 0 0 0) 0))

(defun ptr-read-ubyte (ptr)
  (logand (ptr-read-long ptr) #xFF))

(defun ptr-read-byte (ptr)
  ;; this let triggering a bug in memory-1.nl
  (let ((v (ptr-read-ubyte ptr)))
    (if (>-unsigned v 127)
        (- v 256)
        v)))

;; (defun ptr-read-byte (ptr)
;;   (ptr-read-ubyte ptr))

(defun ptr-read-ushort (ptr)
  (logand (ptr-read-long ptr) #xFFFF))

(defun ptr-read-short (ptr)
  (let ((v (ptr-read-ushort ptr)))
    (if (>-unsigned v #x7FFF)
        (- v #x10000)
        v)))

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

(defun ptr-write-ushort (c ptr)
  (ptr-write-ulong (logior (logand (ptr-read-ulong ptr) #xFFFF0000)
                           (logand c #xFFFF))
                   ptr)
  (+ ptr 2))

(defun ptr-write-short (c ptr)
  (ptr-write-ushort c ptr))

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

(defun ptr-write-quad (value offset)
  (ptr-write-long (make-ulong value value value value) offset))

(defun ptr-set-ulong (offset count &optional (value 0))
  (if (> count 0)
      (progn
        (ptr-write-ulong value offset)
        (ptr-set-ulong (+ offset *SIZEOF_LONG*) (- count 1) value))
      offset))

#+:repl
(defun ptr-read-ptr (ptr)
  (ptr-read-ulong ptr))

#+:repl
(defun ptr-write-ptr (value offset)
  (ptr-write-ulong value offset))

#+:repl
(defun ptr-read-pointer (ptr)
  (ptr-read-ulong ptr))

#+:repl
(defun ptr-write-pointer (value offset)
  (ptr-write-ulong value offset))

(defun ptr-set (offset count &optional (value 0))
  (if (> count 4)
      (progn
        (ptr-write-quad value offset)
        (ptr-set (+ offset *SIZEOF_LONG*) (- count *SIZEOF_LONG*) value))
      (if (> count 0)
          (progn
            (ptr-write-byte value offset)
            (ptr-set (+ offset 1) (- count 1) value))
          offset)))

(defun ptr-copy (src dest count)
  (if (> count *SIZEOF_LONG*)
      (progn
        (ptr-write-long (ptr-read-long src) dest)
        (ptr-copy (+ src *SIZEOF_LONG*) (+ dest *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
      (if (> count 0)
          (progn
            (ptr-write-byte (ptr-read-byte src) dest)
            (ptr-copy (+ src 1) (+ dest 1) (- count 1)))
          dest)))
