;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/string")

(var node-console-base-addr #xF0000100)
(var node-console-memory-size 128)
(var node-console-flush-addr #xF0000100)
(var node-console-buffer-addr #xF0000104)

(defun node-console-write (str &optional (n (length str)))
  (let ((num (if (> n node-console-memory-size)
                 node-console-memory-size
                 n)))
    (ptr-copy str node-console-buffer-addr num)
    num))

(defun node-console-flush (&optional (n node-console-memory-size))
  (ptr-write-long n node-console-flush-addr))
