;;; -*- mode: Lisp; coding: utf-8-unix -*-

#+:sbcl
(in-package :repl)

(require "cpu")

#+:repl (require "runtime/eq")
#+:repl (require "runtime/logic")
#+:repl (require "runtime/error")
#+:repl (require "runtime/math")

(defvar *SIZEOF_BYTE* 1)
(defvar *SIZEOF_SHORT* 2)
(defvar *SIZEOF_USHORT* 2)
(defvar *SIZEOF_POINTER* 4)
(defvar *SIZEOF_LONG* 4)
(defvar *SIZEOF_ULONG* 4)
(defvar *SIZEOF_FLOAT* 4)

#+:sbcl (require "conditions")
#+:repl (require "runtime/error")

#+:sbcl
(define-condition unknown-data-type-error (repl-error)
  ((type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Unknown data type: ~A~%" (slot-value condition 'type)))))

(defun type-atom? (type)
  (cond
    ((eq type :pointer) t)
    ((eq type :byte) t)
    ((eq type :short) t)
    ((eq type :long) t)
    ((eq type :float) t)
    ((eq type :ubyte) t)
    ((eq type :ushort) t)
    ((eq type :ulong) t)
    (t nil)))

(defun type-size (type)
  (cond
    ((or (eq type :long) (or (eq type :ulong) (eq type :pointer)))
     *SIZEOF_LONG*)
    ((or (eq type :short) (eq type :ushort))
     *SIZEOF_SHORT*)
    ((or (eq type :byte) (eq type :ubyte))
     *SIZEOF_BYTE*)
    ((eq type :float)
     *SIZEOF_FLOAT*)
    ((eq type nil)
     0)
    (t (error 'unknown-data-type-error :type type))))
