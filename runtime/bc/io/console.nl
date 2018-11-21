;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/string")
(require "runtime/itoa")
(require "runtime/bc/address-map")

(var console-memory-size 1024)
(var console-buffer-addr console-base-addr)
(var console-flush-addr (+ console-base-addr console-memory-size))

(defun console-write-string (str &optional (n (length str)))
  (let ((num (if (> n console-memory-size)
                 console-memory-size
                 n)))
    (ptr-copy str console-buffer-addr num)
    num))

(defun console-flush (&optional (n console-memory-size))
  (ptr-write-long n console-flush-addr))

(defun console-write (str &optional (n (length str)))
  (let ((num (console-write-string str n)))
    (console-flush num)
    num))

(defun console-write-byte (c)
  (ptr-write-ubyte 0 (ptr-write-ubyte c console-buffer-addr))
  (console-flush 1))

(defun console-write-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa n str base)
    (console-write str)
    n))

(defun console-write-unsigned-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa-unsigned n str base)
    (console-write str)
    n))
