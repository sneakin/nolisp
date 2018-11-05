;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:repl (require "runtime/number")
#+:repl (require "runtime/string")
#+:repl (require "runtime/itoa")

#+:sbcl
(defun itoa (n output-seq &optional (base 10))
  (let ((old-base *print-base*))
    (setq *print-base* base)
    (let ((str (format nil "~A" n)))
      (setq *print-base* old-base)
      (ptr-write-string str output-seq)
      str)))

#+:sbcl
(defun string-aref (str n)
  (aref str n))

(defun string-concat (a b output)
  (ptr-write-string b (ptr-write-string a output)))
