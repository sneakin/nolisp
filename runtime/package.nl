;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defvar *package* :nl-runtime)

(defun in-package (pkg)
  (set *package* pkg))
