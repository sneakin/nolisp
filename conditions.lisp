;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#-:repl
(define-condition repl-error (simple-error)
  ((offset :initarg :offset :initform nil)
   (msg :initarg :msg :initform nil)))

#-:repl
(defmethod print-object ((condition repl-error) stream)
  (format stream "ERROR: ~A at offset ~A" (type-of condition) (slot-value condition 'offset))
  (if (slot-value condition 'msg)
      (format stream ": ~A~%" (slot-value condition 'msg))
      (format stream "~%"))
  (if (slot-value condition 'offset)
      (progn
        (format stream "Memory:~%~A~%" (ptr-read-string (- (slot-value condition 'offset) 32) 32))
        (format stream "> ~A~%" (ptr-read-string (slot-value condition 'offset) 160)))))


#-:repl
(define-condition unknown-op-error (repl-error)
  ((op :initarg :op :initform nil)))

#-:repl
(defmethod print-object ((condition unknown-op-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'op)))
  (call-next-method))

#-:repl
(define-condition not-implemented-error (repl-error)
  ((feature :initarg :feature :initform nil)))

#-:repl
(defmethod print-object ((condition not-implemented-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'feature)))
  (call-next-method))

#-:repl
(define-condition unknown-special-form-error (repl-error)
  ((form :initarg :form :initform nil)))

#-:repl
(defmethod print-object ((condition unknown-special-form-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'form)))
  (call-next-method))

#-:repl
(define-condition malformed-error (repl-error)
  ((form :initarg :form :initform nil)))

#-:repl
(defmethod print-object ((condition malformed-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'form)))
  (call-next-method))

#-:repl
(define-condition malformed-let-error (malformed-error) ())
#-:repl
(define-condition malformed-lambda-error (malformed-error) ())

#-:repl
(define-condition invalid-token-error (malformed-error)
  ((kind :initarg :kind :initform nil)
   (value :initarg :value :initform nil)))

#-:repl
(defmethod print-object ((condition invalid-token-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A \"~A\""
                                              (slot-value condition 'kind)
                                              (slot-value condition 'value))))
  (call-next-method))

#-:repl
(define-condition invalid-escape-error (malformed-error)
  ((char :initarg :char :initform nil)))

#-:repl
(defmethod print-object ((condition invalid-escape-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "\"~A\" \"~A\""
                                              (slot-value condition 'char)
                                              (code-char (slot-value condition 'char)))))
  (call-next-method))

#-:repl
(define-condition invalid-character-error (malformed-error)
  ((value :initarg :char :initform nil)))

#-:repl
(defmethod print-object ((condition invalid-character-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'value)))
  (call-next-method))

#-:repl
(define-condition undefined-error (repl-error)
  ((name :initarg :name :initform nil)))

#-:repl
(defmethod print-object ((condition undefined-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A ~A"
                                              (slot-value condition 'name)
                                              (symbol-string (slot-value condition 'name)))))
  (call-next-method))

#-:repl
(define-condition undefined-variable-error (undefined-error)
  ())

#-:repl
(define-condition undefined-function-error (undefined-error)
  ((args :initarg :args :initform 0)))

#-:repl
(defmethod print-object ((condition undefined-function-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (format nil "~A ~A with ~A arguments"
                                              (slot-value condition 'name)
                                              (symbol-string (slot-value condition 'name))
                                              (slot-value condition 'args))))
  (call-next-method))

#-:repl
(define-condition argument-error (repl-error)
  ((number :initarg :number :initform nil)))

#-:repl
(defmethod print-object ((condition argument-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (slot-value condition 'number)))
  (call-next-method))

#-:repl
(define-condition no-file-error (repl-error)
  ((path :initarg :path :initform nil)))

#-:repl
(defmethod print-object ((condition no-file-error) stream)
  (unless (slot-value condition 'msg)
    (setf (slot-value condition 'msg) (ptr-read-string (slot-value condition 'path))))
  (call-next-method))

