;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:repl (require "runtime/number")
#+:repl (require "runtime/string")
#+:repl (require "runtime/itoa")

#+:sbcl
(defun itoa (n output-seq &optional (base 10))
  (let ((old-base *print-base*))
    (unwind-protect
         (progn (setq *print-base* base)
                (let ((str (format nil "~A" n)))
                  (ptr-write-string str output-seq)
                  str))
      (setq *print-base* old-base))))

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
