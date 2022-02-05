(in-package :nolisp)

(define-macro LET (bindings &rest body)
  `((lambda ,(mapcar #'first bindings)
              ,@body)
            ,@(mapcar #'second bindings)))
