;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(defun symbol-id (symbol-offset &optional (segment *TOKEN-SEGMENT*))
  (ptr-find-string-equal (ptr-read-string symbol-offset) segment symbol-offset))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string symbol-offset)))

(defun symbol-intern (str start ending)
  (if (symbolp str)
      (if (keywordp str)
          (setf str (concatenate 'string ":" (symbol-name str)))
        (setf str (symbol-name str))))
  (let* ((off (ptr-write-string str ending))
         (id (symbol-id ending start)))
    (if id
        (values id ending)
      (values ending off))))

