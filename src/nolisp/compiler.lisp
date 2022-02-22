;;;
;;; Compiler API
;;;

;;; todo defmacro to add macros

(in-package :nolisp)

(defvar *stages* `((macro-expand ,#'macro-expand ,*macros*)
		   (cps-transform ,#'cps-transform)
		   (lookup-resolver ,#'lookup-resolver)
		   (forthgen ,#'forthgen)
		   (flatten ,#'flatten)
		   (to-string ,#'to-string)))

(defun compile-reducer (input stages states &optional new-states)
  (if stages
      (multiple-value-bind (output new-state)
	  (if (first states)
	      (funcall (second (first stages)) input (first states))
	      (funcall (second (first stages)) input))
       (compile-reducer output
			(rest stages)
			(rest states)
			(cons new-state new-states)))
    (values input (nreverse new-states))))

(defun compile-to-forth (form &optional to-stage (stages *stages*))
  (let ((stages (copy-assoc-until-n to-stage stages)))
    (compile-reducer form stages (mapcar #'third stages))))

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
