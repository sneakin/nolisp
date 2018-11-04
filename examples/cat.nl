;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/input-dev")
(require "runtime/bc/io/output-dev")

(defun print-input ()
  (if (and (input-dev-eos) (eq 0 (input-dev-bytes-read)))
      0
      (progn
        (output-dev-write input-dev-buffer-buffer-addr (input-dev-bytes-read))
        (input-dev-wait)
        (print-input))))

(print-input)
