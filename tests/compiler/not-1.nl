;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/halt")
(require "runtime/logic")
(require "runtime/eq")

(values (not 0) (not 1) (not (not 0)) (not 0.0) (not 1.0) 1234)
(halt)
