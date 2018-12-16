;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl
(in-package :repl)

(require "memory")

;; todo merge with string-position, position

#-:sbcl
(defun index-of (needle haystack)
  (let ((ptr (pointer-of needle haystack)))
    (if ptr
        (- ptr haystack)
        -1)))

#+:sbcl
(defun index-of (needle haystack &optional (n 0))
  (if (< n (length haystack))
      (if (eq needle (char-code (aref haystack n)))
          n
          (index-of needle haystack (+ 1 n)))
      -1))
