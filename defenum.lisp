;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(require "symbol")

(define-condition enum-error (simple-error)
  ((enum :initform nil :initarg :enum)
   (name :initform nil :initarg :name)))

(defmethod print-object ((condition enum-error) stream)
  (format stream "~A for enum ~A: ~A~%"
          (type-of condition)
          (slot-value condition 'enum)
          (slot-value condition 'name)))

(define-condition unknown-enum-name (enum-error) ())
(define-condition unknown-enum-value (enum-error) ())

(defun defenum-gen-constant (name value)
  (let ((value-name (first value))
        (value-value (second value)))
    `(defconstant ,(repl::symbol-concat name '- value-name) ,value-value)))

(defun defenum-gen-constants (name values &optional acc)
  (if values
      (defenum-gen-constants
        name
        (rest values)
        (cons (defenum-gen-constant name (first values)) acc))
      acc))

(defun defenum-namer-case (sym name value-def)
  `((eq ,sym ,(second value-def)) ',(repl::symbol-concat name '- (first value-def))))

(defun defenum-gen-namer (name values &optional acc (val-sym (gensym)))
  (if values
      (defenum-gen-namer
          name
          (rest values)
          (cons (defenum-namer-case val-sym name (first values)) acc)
          val-sym)
      `(defun ,(repl::symbol-concat name '- 'name) (,val-sym)
         (cond ,@acc
               (t (error 'unknown-enum-value :enum ',name :name ,val-sym))))))

(defun defenum-stringer-case (sym name value-def)
  `((eq ,sym ,(second value-def)) ,(symbol-name (first value-def))))

(defun defenum-gen-stringer (name values &optional acc (val-sym (gensym)))
  (if values
      (defenum-gen-stringer
          name
          (rest values)
          (cons (defenum-stringer-case val-sym name (first values)) acc)
          val-sym)
      `(defun ,(repl::symbol-concat name '- 'string) (,val-sym)
         (cond ,@acc
               (t (error 'unknown-enum-value :enum ',name :name ,val-sym))))))

(defun defenum-gen-numberer-string-case (sym name value-def)
  `((and (stringp ,sym)
         (string-equal ,sym ,(symbol-name (first value-def)))) ,(second value-def)))

(defun defenum-gen-numberer-symbol-case (sym name value-def)
  `((and (symbolp ,sym)
         (string-equal (symbol-name ,sym) ,(symbol-name (first value-def)))) ,(second value-def)))

(defun defenum-gen-numberer-eq-case (sym name value-def)
  `((eq ,sym ,(second value-def)) ,(second value-def)))

(defun defenum-gen-numberer-cases (name values val-sym gen &optional acc)
  (if values
      (defenum-gen-numberer-cases
          name
          (rest values)
        val-sym
        gen
          (cons (funcall gen val-sym name (first values)) acc))
      acc))

(defun defenum-gen-numberer (name values &optional (val-sym (gensym)))
  (let ((string-cases (defenum-gen-numberer-cases name values val-sym #'defenum-gen-numberer-string-case))
        (symbol-cases (defenum-gen-numberer-cases name values val-sym #'defenum-gen-numberer-symbol-case))
        (eq-cases (defenum-gen-numberer-cases name values val-sym #'defenum-gen-numberer-eq-case))
        )
    `(defun ,(repl::symbol-concat name '- 'number) (,val-sym)
       (cond ,@eq-cases
             ,@string-cases
             ,@symbol-cases
             (t (error 'unknown-enum-name :enum ',name :name ,val-sym))))))

(defmacro defenum (name &rest values)
  `(progn
     ,@(defenum-gen-constants name values)
     ,(defenum-gen-namer name values)
     ,(defenum-gen-stringer name values)
     ,(defenum-gen-numberer name values)))
