(defgeneric nc-error-msg (obj))
(defgeneric nc-error-form (obj))
(defgeneric nc-error-state (obj))

(define-condition nc-error (error)
  ((msg :initarg :msg :initform "Error")
   (form :initarg :form :initform nil)
   (state :initarg :state :initform nil))
  (:report (lambda (condition stream)
             (format stream "~s:~%Form: ~S~%State: ~S~%"
                     (nc-error-msg condition)
                     (nc-error-form condition)
                     (nc-error-state condition)))))
