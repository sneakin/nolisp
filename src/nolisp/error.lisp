(in-package :nolisp)

(defgeneric error-msg (obj))
(defgeneric error-form (obj))
(defgeneric error-state (obj))

(define-condition nolisp-error (error)
  ((msg :initarg :msg :initform "Error")
   (form :initarg :form :initform nil)
   (state :initarg :state :initform nil))
  (:report (lambda (condition stream)
             (format stream "~s:~%Form: ~S~%State: ~S~%"
                     (error-msg condition)
                     (error-form condition)
                     (error-state condition)))))
