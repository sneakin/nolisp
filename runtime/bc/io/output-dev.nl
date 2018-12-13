;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/string")
(require "runtime/itoa")
(require "runtime/halt")
(require "runtime/interrupts")
(require "runtime/bc/address-map")

(var output-dev-memory-size 1024)
(var output-dev-eos-addr output-dev-base-addr)
(var output-dev-cmd-addr (+ output-dev-eos-addr 4))
(var output-dev-buffer-addr (+ output-dev-cmd-addr 4))
(var output-dev-flush-addr (+ output-dev-buffer-addr output-dev-memory-size))

(define-isr output-dev-isr
    (wakeup))

(defun output-dev-init ()
  (interrupts-install output-dev-interrupt output-dev-isr))

(defun output-dev-write-string (str &optional (n (length str)))
  (let ((num (if (> n output-dev-memory-size)
                 output-dev-memory-size
                 n)))
    (ptr-copy str output-dev-buffer-addr num)
    num))

(defun output-dev-eos ()
  (ptr-read-ulong output-dev-eos-addr))

(defun output-dev-wait (&optional (disabled (not (interrupts-enabled?))))
  (if (= (output-dev-eos) 0) ;; EOS == ok
      t
      (if (= (output-dev-eos) 4) ;; EOS == FULL
          (progn
            (interrupts-enable)
            (sleep)
            (if disabled (interrupts-disable))
            (output-dev-wait disabled))
          nil)))

#+:never
(defun output-dev-flush (&optional (n output-dev-memory-size))
  (if (output-dev-wait)
      (ptr-write-long n output-dev-flush-addr)
      nil))

(defun output-dev-flush (&optional (n output-dev-memory-size))
  (ptr-write-long n output-dev-flush-addr)
  n)

(defun output-dev-write (str &optional (n (length str)))
  (let ((num (output-dev-write-string str n)))
    (if (output-dev-flush num)
        num
        nil)))

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
