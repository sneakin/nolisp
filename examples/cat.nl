;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/node_console")

(defun print-input ()
  (if (and (node-input-eos) (eq 0 (node-input-bytes-read)))
      0
      (progn
        (node-output-write node-input-buffer-buffer-addr (node-input-bytes-read))
        (node-input-wait)
        (print-input))))

(print-input)
