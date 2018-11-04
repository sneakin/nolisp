;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/logic")
(require "runtime/math")
(require "runtime/cmp")

(var input-dev-base-addr #xF0003000)
(var input-dev-memory-size 1024)
(var input-dev-read-addr input-dev-base-addr)
(var input-dev-eos-addr (+ input-dev-read-addr 4))
(var input-dev-buffer-read-offset-addr (+ input-dev-eos-addr 4))
(var input-dev-buffer-write-offset-addr (+ input-dev-buffer-read-offset-addr 4))
(var input-dev-buffer-buffer-addr (+ input-dev-eos-addr 4))

(defun input-dev-bytes-read ()
  (ptr-read-ulong input-dev-read-addr))

(defun input-dev-eos ()
  (ptr-read-ulong input-dev-eos-addr))

(defun input-dev-ready ()
  (> (input-dev-bytes-read) 0))

(defun input-dev-get (offset)
  (ptr-read-ubyte (+ input-dev-buffer-buffer-addr offset)))

(var input-dev-next-byte 0)

(defun input-dev-read-more ()
  (ptr-write-ulong 0 input-dev-read-addr))

;; todo wait using SLEEP and interrupt
(defun input-dev-wait ()
  (input-dev-read-more)
  (if (or (> (input-dev-bytes-read) 0)
          (input-dev-eos))
      t
      (input-dev-wait)))

(defun input-dev-read-next ()
  (if (and (input-dev-eos)
           (>= input-dev-next-byte (input-dev-bytes-read)))
      0
      (progn
        (if (>= input-dev-next-byte (input-dev-bytes-read))
            (input-dev-wait)
            (set input-dev-next-byte 0))
        (let ((b (input-dev-get input-dev-next-byte)))
          (set input-dev-next-byte (+ input-dev-next-byte 1))          
          b))))
