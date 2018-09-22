;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "type-sizes")

(defun aref (array n)
  (ptr-read-long (+ array (* n *SIZEOF_LONG*))))
