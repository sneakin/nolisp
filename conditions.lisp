(in-package :repl)

#+:sbcl
(define-condition repl-error ()
  ((offset :initarg :offset :initform nil)
   (msg :initarg :msg :initform nil))
  (:report (lambda (condition stream)
             (format stream "Error at offset ~A~%" (slot-value condition 'offset))
             (if (slot-value condition 'msg)
                 (format stream (slot-value condition 'msg))))))

#+:sbcl
(define-condition unknown-op-error (repl-error)
  ((op :initarg :op :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown op error: ~A ~A~%" (slot-value condition 'op) (type-of (slot-value condition 'op))))))

#+:sbcl
(define-condition undefined-variable-error (repl-error)
  ((variable :initarg :variable :initform nil)
   (offset :initarg :offset :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "Undefined symbol at ~A: ~A ~A~%"
                     (slot-value condition 'offset)
                     (symbol-string (slot-value condition 'variable))
                     (slot-value condition 'variable)))))

#+:sbcl
(define-condition not-implemented-error (repl-error)
  ((feature :initarg :feature :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A not implemented at ~A~%"
                     (slot-value condition 'feature)
                     (slot-value condition 'offset)))))

#+:sbcl
(define-condition unknown-special-form-error (repl-error)
  ((form :initarg :form :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown special form at ~A: ~A~%"
                     (slot-value condition 'offset)
                     (slot-value condition 'form)))))

#+:sbcl
(define-condition malformed-error (repl-error)
  ((form :initarg :form :initform nil))
  (:report (lambda (condition stream)
             (format stream "Malformed form at ~A: ~A~%"
                     (slot-value condition 'offset)
                     (or (slot-value condition 'form) (ptr-read-string (slot-value condition 'offset)))))))

#+:sbcl
(define-condition malformed-let-error (malformed-error) ())
#+:sbcl
(define-condition malformed-lambda-error (malformed-error) ())

#+:sbcl
(define-condition invalid-token-error (malformed-error)
  ((kind :initarg :kind :initform nil)
   (value :initarg :value :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A token at ~A: ~A~%~A~%"
                     (slot-value condition 'kind)
                     (slot-value condition 'offset)
                     (slot-value condition 'value)
                     (ptr-read-string (slot-value condition 'offset))))))

#+:sbcl
(define-condition invalid-escape-error (malformed-error)
  ((char :initarg :char :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid character escape ~A: ~A~%"
                     (slot-value condition 'offset)
                     (slot-value condition 'char)))))

#+:sbcl
(define-condition invalid-character-error (malformed-error)
  ((value :initarg :char :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid character ~A: ~A~%"
                     (slot-value condition 'offset)
                     (slot-value condition 'value)))))

#+:sbcl
(define-condition undefined-error (repl-error)
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (format stream "Undefined variable ~A~%"
                     (slot-value condition 'name)))))

#+:sbcl
(define-condition undefined-variable-error (undefined-error) ())
#+:sbcl
(define-condition undefined-function-error (undefined-error) ())
