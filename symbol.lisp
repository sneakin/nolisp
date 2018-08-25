;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(defun symbol-id (symbol-offset &optional (segment *TOKEN-SEGMENT*))
  (ptr-find-string-equal (ptr-read-string symbol-offset) segment symbol-offset))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string symbol-offset)))

(defun keyword? (sym)
  (eq (ptr-read-byte sym)
      (char-code #\:)))

(defun symbol-intern (str segment-start segment-end)
  (if (symbolp str)
      (if (keywordp str)
          (setf str (concatenate 'string ":" (symbol-name str)))
          (setf str (symbol-name str))))
  (let* ((off (ptr-write-string str segment-end))
         (id (symbol-id segment-end segment-start)))
    (if id
        (values id segment-end)
        (values segment-end off))))

(defvar *symbol-gen-prefix* "-sym-")
(defvar *symbol-next-token* 0)

(defun symbol-gen (offset &optional (segment *TOKEN-SEGMENT*))
  (multiple-value-bind (sym new-ending)
      (symbol-intern (concatenate 'string *symbol-gen-prefix* (iota *symbol-next-token*))
                     segment offset)
    (incf *symbol-next-token* 1)
    (values sym new-ending)))

