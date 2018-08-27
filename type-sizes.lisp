;;; -*- mode: Lisp; coding: utf-8-unix -*-

#+:sbcl
(in-package :repl)

(defvar *SIZEOF_BYTE* 2)
(defvar *SIZEOF_SHORT* 2)
(defvar *SIZEOF_USHORT* 2)
(defvar *SIZEOF_LONG* 4)
(defvar *SIZEOF_ULONG* 4)
(defvar *SIZEOF_FLOAT* 4)
(defvar *REGISTER-SIZE* *SIZEOF_LONG*)

#+:sbcl
(define-condition unknown-data-type-error (repl-error)
  ((type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Unknown data type: ~A~%" (slot-value condition 'type)))))

(defun type-size (type)
  (cond
    ((or (eq type :long) (eq type :ulong))
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
