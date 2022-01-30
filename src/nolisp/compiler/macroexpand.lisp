;;;
;;; Macroexpand
;;;

(in-package :nolisp)

(defvar *macros* '())

(defun macro-list (macros) (or macros *macros*))
(defun macros-global? (macros) (eq macros nil))

(defun macro? (form &optional macros)
  (not (not (assoc form (macro-list macros)))))

(defun remove-macro (name &optional macros)
  (let ((macro (assoc name (macro-list macros))))
    (if macro
	(let ((lst (remove macro (macro-list macros))))
	  (if (macros-global? macros) (setq *macros* lst))
	  lst)
      macros)))

(defun add-macro (name fn &optional macros)
  (let ((lst (acons name fn (macro-list macros))))
    (if (macros-global? macros) (setf *macros* lst))
    lst))

(defun update-macro (name fn &optional macros)
  (let ((lst (add-macro name fn (remove-macro name macros))))
    (if (macros-global? macros) (setf *macros* lst))
    lst))

(defmacro define-macro (name arglist &rest body)
  `(update-macro ',(intern (symbol-name name) :cl-user)
		 #'(lambda ,arglist ,@body)))

(defun call-macro (macros form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name (macro-list macros))))
    (if macro
        (apply (cdr macro) args)
        (mapcar visitor form))))

(defun macro-expand-1 (form &optional macros)
  (scan-list form
             #'(lambda (a state) a)
             (partial-first #'call-macro macros)))

(defun macro-expand (form &optional macros)
  (let ((new-form (macro-expand-1 form macros)))
    (if (equal form new-form)
        form
        (macro-expand new-form macros))))

(define-macro LET (bindings &rest body)
  `((lambda ,(mapcar #'first bindings)
              ,@body)
            ,@(mapcar #'second bindings)))
