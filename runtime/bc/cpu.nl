;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl (in-package :repl)
#+:repl (require "runtime/eq")

(defvar *REGISTER-SIZE* 4)
(defvar *REGISTER-COUNT* 16)

(defvar *REGISTER-INS* 15)
(defvar *REGISTER-STATUS* 14)
(defvar *REGISTER-ISR* 13)
(defvar *REGISTER-IP* 12)
(defvar *REGISTER-SP* 11)
(defvar *REGISTER-CS* 10)
(defvar *REGISTER-DS* 9)

(defun system-register? (reg)
  (cond
    ((eq reg *REGISTER-INS*) t)
    ((eq reg *REGISTER-STATUS*) t)
    ((eq reg *REGISTER-ISR*) t)
    ((eq reg *REGISTER-IP*) t)
    ((eq reg *REGISTER-SP*) t)
    (t nil)))
