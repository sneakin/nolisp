;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl (in-package :repl)

#+:repl
(progn
  (defvar *CODE-SEGMENT*)
  (defvar *STRING-SEGMENT*)
  (defvar *COMPILER*))

#-:repl
(progn
  (defvar *CODE-SEGMENT* 0)
  (defvar *STRING-SEGMENT* 0)
  (defvar *COMPILER* 0))

(defvar *INPUT-BASE* 10)
(defvar *output-base* 10)
