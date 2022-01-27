;;;
;;; Compiler API
;;;

;;; todo defmacro to add macros

(in-package :nolisp)

(defvar *stages* `((macro-expand . ,#'macro-expand)
		   (cps-transform . ,#'cps-transform)
		   (lookup-resolver . ,#'lookup-resolver)
		   (forthgen . ,#'forthgen)
		   (flatten . ,#'flatten)
		   (to-string . ,#'to-string)))

(defun compile-to-forth (form &optional to-stage (stages *stages*) last-stage)
  (if (numberp to-stage)
      (compile-to-forth form (car (nth to-stage stages)) stages)
    (if (and stages (not (and last-stage (eq to-stage last-stage))))
	(compile-to-forth (funcall (cdr (first stages)) form)
			  to-stage
			  (rest stages)
			  (first (first stages)))
      form)))

;;;
;;; Helper ~compile-to-forth~ wrappers:
;;;

(defun compile-to-lookup (form)
  (compile-to-forth form 'lookup-resolver))

(defun compile-to-list (form)
  (compile-to-forth form 'forthgen))

(defun compile-to-flatlist (form)
  (compile-to-forth form 'flatten))

(defun compile-to-string (form)
  (compile-to-forth form 'to-string))
