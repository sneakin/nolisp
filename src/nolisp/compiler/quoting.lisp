(in-package :nolisp)

(defun quoted? (form) (eq 'quote (first form)))

#+sbcl
(progn
  (defun quasiquoted? (form) (eq 'sb-int:quasiquote (first form)))
  (defun unquoted? (form) (or (and (eq 'sb-impl::comma (type-of form))
				   (eq 0 (slot-value form 'sb-impl::kind)))
			      (and (listp form)
			           (eq 'CL-USER::UNQUOTE (first form)))))
  (defun unquoted-splice? (form) (or (and (eq 'sb-impl::comma (type-of form))
					  (eq 2 (slot-value form 'sb-impl::kind)))
				     (and (listp form)
				          (eq 'CL-USER::UNQUOTE-SPLICE (first form)))))
  (defun unquoted-value (form) (if (eq 'sb-impl::comma (type-of form))
				   (slot-value form 'sb-impl::expr)
				   (second form))))
#+ecl
(progn
  (defun quasiquoted? (form) (eq 'SI::QUASIQUOTE (first form)))
  (defun unquoted? (form) (and (listp form)
                               (or (eq 'SI::UNQUOTE (first form))
			           (eq 'CL-USER::UNQUOTE (first form)))))
  (defun unquoted-splice? (form) (and (listp form)
				      (or (eq 'SI::UNQUOTE-SPLICE (first form))
					  (eq 'CL-USER::UNQUOTE-SPLICE (first form)))))
  (defun unquoted-value (form) (second form)))
#+(and -sbcl -ecl)
(error 'simple-error :format-control "Quasiquoting needs helper functions defined.")

(defun unquote-list (form &optional acc)
  (cond
    ((eq nil form) (if acc (cons 'list (nreverse acc)) nil))
    ((symbolp form) (list 'quote form))
    ((atom form) form)
    ((and (rest form) (atom (rest form)))
     (let ((cell (list 'cons
		       (unquote-list (first form))
		       (unquote-list (rest form)))))
       (if acc
	   (cons 'list* (nreverse (cons cell acc)))
	   cell)))
    (t (unquote-list (rest form) (cons (unquote-list (first form)) acc)))))

(defun quasiquote-list (form &optional acc)
  (cond
    ((eq nil form) (if acc (cons 'list (nreverse acc)) nil))
    ((symbolp form) (list 'quote form))
    ((unquoted? form) (unquoted-value form))
    ((unquoted-splice? form) (unquoted-value form))
    ((atom form) form)
    ((and (rest form) (atom (rest form)))
     (let ((cell (list 'cons
		       (quasiquote-list (first form))
		       (quasiquote-list (rest form)))))
       (if acc
	   (cons 'list* (nreverse (cons cell acc)))
	   cell)))
    (t (let ((result (quasiquote-list (first form))))
	 (quasiquote-list (rest form)
			  (if (and (listp result)
				   (unquoted-splice? (first form)))
			      (nconc (nreverse result) acc)
			      (cons result acc)))))))

(defun quoting-list (form visitor state)
  (declare (ignore state))
  (cond
    ((quoted? form) (unquote-list (second form)))
    ((quasiquoted? form) (quasiquote-list (second form)))
    (t (map-improper visitor form))))

(defun quoting-atom (atom state)
  (declare (ignore state))
  atom)

(defun quoting-transform (form &optional state)
  (scan-list form #'quoting-atom #'quoting-list state))
