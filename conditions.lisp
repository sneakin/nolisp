;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:sbcl
(define-condition repl-error (simple-error)
  ((offset :initarg :offset :initform nil)
   (msg :initarg :msg :initform nil)))

#+:sbcl
(defmethod print-object ((condition repl-error) stream)
  (format stream "ERROR: ~A at offset ~A" (type-of condition) (slot-value condition 'offset))
  (if (slot-value condition 'msg)
      (format stream ": ~A~%" (slot-value condition 'msg))
      (format stream "~%"))
  (if (slot-value condition 'offset)
      (progn
        (format stream "Memory:~%~A~%" (ptr-read-string (- (slot-value condition 'offset) 32) 32))
        (format stream "> ~A~%" (ptr-read-string (slot-value condition 'offset) 160)))))


#+:sbcl
(define-condition unknown-op-error (repl-error)
  ((op :initarg :op :initform nil)))

#+:sbcl
(defmethod print-object ((condition unknown-op-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'op)))
  (call-next-method))

#+:sbcl
(define-condition not-implemented-error (repl-error)
  ((feature :initarg :feature :initform nil)))

#+:sbcl
(defmethod print-object ((condition not-implemented-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'feature)))
  (call-next-method))

#+:sbcl
(define-condition unknown-special-form-error (repl-error)
  ((form :initarg :form :initform nil)))

#+:sbcl
(defmethod print-object ((condition unknown-special-form-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'form)))
  (call-next-method))

#+:sbcl
(define-condition malformed-error (repl-error)
  ((form :initarg :form :initform nil)))

#+:sbcl
(defmethod print-object ((condition malformed-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'form)))
  (call-next-method))

#+:sbcl
(define-condition malformed-let-error (malformed-error) ())
#+:sbcl
(define-condition malformed-lambda-error (malformed-error) ())

#+:sbcl
(define-condition invalid-token-error (malformed-error)
  ((kind :initarg :kind :initform nil)
   (value :initarg :value :initform nil)))

#+:sbcl
(defmethod print-object ((condition invalid-token-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A \"~A\""
                                              (slot-value condition 'kind)
                                              (slot-value condition 'value))))
  (call-next-method))

#+:sbcl
(define-condition invalid-escape-error (malformed-error)
  ((char :initarg :char :initform nil)))

#+:sbcl
(defmethod print-object ((condition invalid-escape-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "\"~A\" \"~A\""
                                              (slot-value condition 'char)
                                              (code-char (slot-value condition 'char)))))
  (call-next-method))

#+:sbcl
(define-condition invalid-character-error (malformed-error)
  ((value :initarg :char :initform nil)))

#+:sbcl
(defmethod print-object ((condition invalid-character-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'value)))
  (call-next-method))

#+:sbcl
(define-condition undefined-error (repl-error)
  ((name :initarg :name :initform nil)))

#+:sbcl
(defmethod print-object ((condition undefined-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A ~A"
                                              (slot-value condition 'name)
                                              (symbol-string (slot-value condition 'name)))))
  (call-next-method))

#+:sbcl
(define-condition undefined-variable-error (undefined-error)
  ())

#+:sbcl
(define-condition undefined-function-error (undefined-error)
  ((args :initarg :args :initform 0)))

#+:sbcl
(defmethod print-object ((condition undefined-function-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A ~A with ~A argugments"
                                              (slot-value condition 'name)
                                              (symbol-string (slot-value condition 'name))
                                              (slot-value condition 'args))))
  (call-next-method))

#+:sbcl
(define-condition argument-error (repl-error)
  ((number :initarg :number :initform nil)))

#+:sbcl
(defmethod print-object ((condition argument-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'number)))
  (call-next-method))

#+:sbcl
(define-condition no-file-error (repl-error)
  ((path :initarg :path :initform nil)))

#+:sbcl
(defmethod print-object ((condition no-file-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (ptr-read-string (slot-value condition 'path))))
  (call-next-method))

