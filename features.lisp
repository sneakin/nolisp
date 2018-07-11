;;; -*- mode: Lisp; coding: utf-8-unix -*-

;;; Info functions

(in-package :repl)

(defvar *repl-features* (list :repl :cross))

(defun has-feature? (feature &optional (features *repl-features*))
  (if (stringp feature)
      (setq feature (intern (string-upcase (if (eq (aref feature 0) #\:)
                                               (subseq feature 1)
                                             feature))
                            "KEYWORD")))
  (not (eq nil (position feature features))))
