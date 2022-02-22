(in-package :nolisp)

;;; Takes a list of lists that are mostly definitions of functions,
;;; macros, variables, and constants. May also have evaluated forms
;;; to generate definitions.

(defun gen-macro-caller (args body)
  (eval `(lambda ,args ,@body)))

(defun toplevel-defmacro (macros name arglist &rest body)
  (update-macro name (gen-macro-caller arglist body) macros))

(defun toplevel-compile-reducer (input acc stages states)
  (multiple-value-bind (output new-states)
      (compile-reducer input stages states)
    (values (list* :newline output acc) stages new-states)))

(defun toplevel-reducer (input acc stages states)
  (cond
    ((atom input)
     (toplevel-compile-reducer input acc stages states))
    ((eq (first input) 'defmacro)
     (let ((it (bassoc 'macro-expand stages states)))
       (rplaca it (apply #'toplevel-defmacro (cons (first it) (rest input))))
       (values acc stages states)))
    (t (toplevel-compile-reducer input acc stages states))))

(defun toplevel-default-states (&optional stages)
  (mapcar #'third (or stages *stages*)))

(defun toplevel-compile (forms &optional (stages *stages*) states)
  (multiple-value-bind (output stages states)
      (reduce-values #'toplevel-reducer
		     forms
		     :initial-value (list
				     nil
				     (or stages *stages*)
				     (or states (toplevel-default-states stages))))
    (declare (ignore stages))
    (values (to-string (nreverse output)) states)))

(defun toplevel-compile-stream (io &optional stages states &key fn)
  (destructuring-bind (final-out states)
      (each-read-expr #'(lambda (ex acc states)
			  (multiple-value-bind (out stages states)
			      (toplevel-reducer ex nil stages states)
			    (declare (ignore stages))
			    (let ((out (to-string (nreverse out))))
			      (if fn (funcall fn out))
			      (values (cons out acc) states))))
		      io
		      (list nil states))
    (values (to-string (nreverse final-out)) states)))

(defun toplevel-compile-file (path &optional stages states &key fn)
  (with-open-file (io path :direction :input)
    (toplevel-compile-stream io stages states :fn fn)))
