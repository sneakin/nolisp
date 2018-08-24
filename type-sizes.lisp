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
  (case type
    (:long *SIZEOF_LONG*)
    (:ulong *SIZEOF_LONG*)
    (:short *SIZEOF_SHORT*)
    (:ushort *SIZEOF_SHORT*)
    (:byte *SIZEOF_BYTE*)
    (:ubyte *SIZEOF_BYTE*)
    (:float *SIZEOF_FLOAT*)
    ((nil) 0)
    (otherwise (error 'unknown-data-type-error :type type))))
