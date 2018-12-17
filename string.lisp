;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:repl (require "runtime/number")
#+:repl (require "runtime/string")
#+:repl (require "runtime/convertors")

#+:sbcl
(defun string-aref (str n)
  (let ((str (ptr-read-string str)))
    (if (>= n (length str))
        0
        (aref str n))))

#-:sbcl
(defun string-concat (a b output)
  (ptr-write-string b (- (ptr-write-string a output) 1)))

#+:sbcl
(defun string-concat (a b output)
  (ptr-write-string b (- (ptr-write-string a output) 1)))

#+:sbcl
(defun string-position (char str &optional (n 0))
  (position char (ptr-read-string str) :start n))
