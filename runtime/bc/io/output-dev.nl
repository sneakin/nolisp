;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/string")
(require "runtime/itoa")

(var output-dev-base-addr #xF0002000)
(var output-dev-memory-size 1024)
(var output-dev-buffer-addr output-dev-base-addr)
(var output-dev-flush-addr (+ output-dev-base-addr output-dev-memory-size))

(defun output-dev-write-string (str &optional (n (length str)))
  (let ((num (if (> n output-dev-memory-size)
                 output-dev-memory-size
                 n)))
    (ptr-copy str output-dev-buffer-addr num)
    num))

(defun output-dev-flush (&optional (n output-dev-memory-size))
  (ptr-write-long n output-dev-flush-addr))

(defun output-dev-write (str &optional (n (length str)))
  (let ((num (output-dev-write-string str n)))
    (output-dev-flush num)
    num))

(defun output-dev-write-byte (c)
  (ptr-write-ubyte 0 (ptr-write-ubyte c output-dev-buffer-addr))
  (output-dev-flush 1))

(defun output-dev-write-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa n str base)
    (output-dev-write str)
    n))

(defun output-dev-write-unsigned-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa-unsigned n str base)
    (output-dev-write str)
    n))
