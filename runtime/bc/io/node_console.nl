;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/string")
(require "runtime/itoa")

(var node-output-base-addr #xF0002000)
(var node-output-memory-size 1024)
(var node-output-buffer-addr node-output-base-addr)
(var node-output-flush-addr (+ node-output-base-addr node-output-memory-size))

(defun node-output-write-string (str &optional (n (length str)))
  (let ((num (if (> n node-output-memory-size)
                 node-output-memory-size
                 n)))
    (ptr-copy str node-output-buffer-addr num)
    num))

(defun node-output-flush (&optional (n node-output-memory-size))
  (ptr-write-long n node-output-flush-addr))

(defun node-output-write (str &optional (n (length str)))
  (let ((num (node-output-write-string str n)))
    (node-output-flush num)
    num))

(defun node-output-write-byte (c)
  (ptr-write-ubyte 0 (ptr-write-ubyte c node-output-buffer-addr))
  (node-output-flush 1))

(defun node-output-write-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa n str base)
    (node-output-write str)
    n))

(defun node-output-write-unsigned-integer (n &optional (base *output-base*))
  (with-allocation (str 36)
    (itoa-unsigned n str base)
    (node-output-write str)
    n))

(var node-input-base-addr #xF0003000)
(var node-input-memory-size 1024)
(var node-input-read-addr node-input-base-addr)
(var node-input-eos-addr (+ node-input-read-addr 4))
(var node-input-buffer-read-offset-addr (+ node-input-eos-addr 4))
(var node-input-buffer-write-offset-addr (+ node-input-buffer-read-offset-addr 4))
(var node-input-buffer-buffer-addr (+ node-input-eos-addr 4))

(defun node-input-bytes-read ()
  (ptr-read-ulong node-input-read-addr))

(defun node-input-eos ()
  (ptr-read-ulong node-input-eos-addr))

(defun node-input-ready ()
  (> (node-input-bytes-read) 0))

(defun node-input-get (offset)
  (ptr-read-ubyte (+ node-input-buffer-buffer-addr offset)))

(var node-input-next-byte 0)

(defun node-input-read-more ()
  (ptr-write-ulong 0 node-input-read-addr))

;; todo wait using SLEEP and interrupt
(defun node-input-wait ()
  (node-input-read-more)
  (if (or (> (node-input-bytes-read) 0)
          (node-input-eos))
      t
      (node-input-wait)))

(defun node-input-read-next ()
  (if (and (node-input-eos)
           (>= node-input-next-byte (node-input-bytes-read)))
      0
      (progn
        (if (>= node-input-next-byte (node-input-bytes-read))
            (node-input-wait)
            (set node-input-next-byte 0))
        (let ((b (node-input-get node-input-next-byte)))
          (set node-input-next-byte (+ node-input-next-byte 1))          
          b))
      ))

