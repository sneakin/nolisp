;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/output-dev")
(require "runtime/bc/io/input-dev")
(require "runtime/halt")

(defun print-buffer-stats ()
  (output-dev-write "\nRead: ")
  (output-dev-write-integer (input-dev-bytes-read))
  (output-dev-write "\nEOS: ")
  (output-dev-write-integer (input-dev-eos))
  (output-dev-write "\nNext byte: ")
  (output-dev-write-integer input-dev-next-byte)
  (output-dev-write "\n"))

(defun dump-input-buffer (&optional (max 32) (n 0))
  (if (< n max)
      (progn
        (output-dev-write "\n")
        (output-dev-write-integer n)
        (output-dev-write ": ")
        (output-dev-write-integer (ptr-read-ubyte (+ input-dev-base-addr n)))
        (dump-input-buffer max (+ n 1)))))

(output-dev-write-integer (count-digits #xF0000000 16))
(output-dev-write "\n")
(output-dev-write-integer 1234 2)
                                        ;(output-dev-write-integer 1234 3)
(output-dev-write-integer -1234 16)
(output-dev-write-integer -1234 10)
(output-dev-write-integer #x-BCD 16)
(output-dev-write-integer #x-BCE)

(output-dev-write-integer #xFFFFFFFF 16)
(output-dev-write-unsigned-integer #xFFFFFFFF 16)
(output-dev-write-unsigned-integer input-dev-base-addr 16)
(print-buffer-stats)
(dump-input-buffer)

(output-dev-write "\nName? ")
(with-allocation (buffer 128)
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (print-buffer-stats)
  (dump-input-buffer)
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  (output-dev-write-integer (input-dev-read-next))
  (output-dev-write "\n")
  ;;(output-dev-write (input-dev-read buffer 128))
  (output-dev-write "\n")
  (input-dev-wait)
  (print-buffer-stats)
  )