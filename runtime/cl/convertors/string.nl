;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(defun itoa (n output-seq &optional (base *print-base*))
  (let ((old-base *print-base*))
    (unwind-protect
         (progn (setq *print-base* base)
                (let ((str (format nil "~A" n)))
                  (ptr-write-string str output-seq)
                  str))
      (setq *print-base* old-base))))
