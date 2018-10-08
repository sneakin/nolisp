;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/halt")
(require "runtime/eq")

(values (eq 0 0) (eq 12 12) (eq 0 1))
(halt)
