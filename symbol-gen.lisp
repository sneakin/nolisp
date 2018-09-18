;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "symbol")
(require "string")

(in-package :repl)

(defvar *symbol-gen-prefix* "-sym-")
(defvar *symbol-next-token* 0)

(defun symbol-gen (offset &optional (segment (package-string-segment-data *COMPILER*)))
  (multiple-value-bind (sym new-ending)
      (symbol-intern (concatenate 'string *symbol-gen-prefix* (itoa *symbol-next-token*))
                     segment offset)
    (incf *symbol-next-token* 1)
    (values sym new-ending)))

