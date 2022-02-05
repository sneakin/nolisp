(in-package :nolisp)

(defun maybe-wrap-with-progn (forms)
  (if (< 1 (length forms))
      `(progn ,@forms)
    (first forms)))

(defun cond-expander (cases)
  (if cases
      (destructuring-bind
       (test &rest body) (first cases)
       (if (eq t test)
	   (maybe-wrap-with-progn body)
	 `(if ,test
	      ,(maybe-wrap-with-progn body)
	    ,(cond-expander (rest cases)))))))

(define-macro COND (&rest cases)
  (cond-expander cases))
