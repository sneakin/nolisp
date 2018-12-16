;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/console")
(require "runtime/bc/io/input-dev")
(require "runtime/bc/io/output-dev")

(defun io-init ()
  (output-dev-init)
  (input-dev-init))
