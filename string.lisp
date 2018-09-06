;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#-:sbcl
(defun string= (a b &optional (as 0) (bs 0))
  (if (or (>= as (length a))
          (>= bs (length b)))
      t
    (if (= (- (length a) as) (- (length b) bs))
        (if (eq (aref a as) (aref b bs))
            (string= a b (+ 1 as) (+ 1 bs))))))

#+:sbcl
(defun itoa (n &optional (base 10))
  (let ((old-base *print-base*))
    (setq *print-base* base)
    (let ((str (format nil "~A" n)))
      (setq *print-base* old-base)
      str)))

#+:sbcl
(defun string-aref (str n)
  (aref str n))
