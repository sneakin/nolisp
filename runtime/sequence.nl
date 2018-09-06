;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun aref (array n)
  (ptr-read-long (+ array (* n *SIZEOF_LONG*))))
