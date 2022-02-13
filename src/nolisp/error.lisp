(in-package :nolisp)

(define-condition nolisp-error (error)
  ((msg :initarg :msg :initform "Error" :reader error-msg)
   (form :initarg :form :initform nil :reader error-form)
   (state :initarg :state :initform nil :reader error-state))
  (:report (lambda (condition stream)
             (format stream "~s:~%Form: ~S~%State: ~S~%"
                     (error-msg condition)
                     (error-form condition)
                     (error-state condition)))))
