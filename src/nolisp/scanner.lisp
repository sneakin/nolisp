(in-package :nolisp)

(define-condition nc-scanner-error (nc-error) ())

;;;
;;; The list scanner
;;;

(defun scan-list (form atom-visitor list-visitor
                  &optional
                    state
                    (recurser #'(lambda (f &optional (s state))
                                  (scan-list f atom-visitor list-visitor s))))
  (cond
    ((atom form) (funcall atom-visitor form state))
    ((listp form) (funcall list-visitor form recurser state))
    (t (error 'nc-scanner-error :form form :state state))))
