;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:repl (require "runtime/number")
#+:repl (require "runtime/string")

;;#-:sbcl
;; (defun string= (a b &optional (as 0) (bs 0))
;;   (if (or (>= as (length a))
;;           (>= bs (length b)))
;;       t
;;     (if (= (- (length a) as) (- (length b) bs))
;;         (if (eq (aref a as) (aref b bs))
;;             (string= a b (+ 1 as) (+ 1 bs))))))

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
