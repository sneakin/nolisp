;;;
;;; CPS Transform
;;;

(in-package :nolisp)

(define-condition cps-transform-error (nolisp-error) ())

(defun cps-atom? (form)
  (or (atom form) (eq 'CL-USER::λ (first form))))

(defun cps-closure? (form)
  (and (listp form) (eq 'CL-USER::λ (first form))))

(defun lambda-form? (form)
  (and (listp form) (eq 'CL-USER::LAMBDA (first form))))

(defun cps-lambda (sym form)
  `(CL-USER::λ (,sym) ,form))

(defun cps-wrap (sym form cc)
  (if (cps-atom? cc)
      `(,@form ,cc)
      `(,@form ,(cps-lambda sym cc))))

(defun cps-transform-call-emit (fns ops visitor &optional (first-call t))
  (if fns
      (let* ((form (rest (first fns)))
             (sym (first (first fns)))
             (new-form (funcall visitor form (cps-lambda sym ops))))
        (cps-transform-call-emit (rest fns)
                                 new-form
                                 visitor
                                 nil))
       ops))

(defun cps-transform-call (form visitor state &optional args fns (sym (gensym "R")))
  (if form
      (let ((tip (first form)))
        (if (cps-atom? tip)
            (cps-transform-call (rest form) visitor state
                                (cons tip args)
                                fns
                                sym)
            (cps-transform-call (rest form) visitor state
                                (cons sym args)
                                (acons sym tip fns)
                                (gensym "R"))))
      (cps-transform-call-emit fns
                               (cps-wrap sym (reverse args) state)
                               visitor)))

(defun cps-emit-if (form visitor cc)
  (funcall visitor (second form)
	   (cps-lambda 'cl-user::test
		       `(if cl-user::test
			    ,(funcall visitor (third form) cc)
			    ,(funcall visitor (fourth form) cc)))))

(defun cps-transform-list (form visitor state)
  (match-case form
    (nil state)
    ;; (if test then else)
    ((IF ?test . ?cases)
     ;; transform IF statements so any caller is placed inside
     ;; a new lambda used as the continuation of the two clauses
     (if (or (eq state nil) (symbolp state) (eq state 'CL-USER::RETURN))
	 (cps-emit-if form visitor state)
       (let ((cc (gensym "CC")))
         `(CL:LAMBDA ,(second state)
		     ,@(cddr state)
	    (CL-USER::λ (,cc) ,(cps-emit-if form visitor cc))))))
    ;; (lambda arglist exprs*)
    ((LAMBDA ?arglist . ?body)
     `(CL:LAMBDA ,arglist
	,(funcall visitor
		  (if (second body)
		      (cons 'progn body)
		      (first body))
		  'CL-USER::RETURN)
	,state))
    ;; (defun name arglist exprs*)
    ((DEFUN ?name ?arglist . ?body)
     `(CL:DEFUN ,name ,arglist
                      ,(funcall visitor (if (second body)
					    (cons 'progn body)
					    (first body)))))
    ;; (progn exprs*)
    ((PROGN . ?body)
      (if (eq nil (second body))
	  (funcall visitor (first body))
	  (cps-transform-call form visitor state)))
    ;; #'name | #'(lambda ...)
    ((FUNCTION ?form)
     (if (lambda-form? form)
	 ;; unwrap lambdas
	 (funcall visitor form)
	 ;; pass through other forms
	 `(CL-USER::FUNCTION ,form ,state)))
    ;; (func args*)
    (t (cps-transform-call form visitor state))))

(defun cps-uncurry (form value)
  (destructuring-bind (kind args exprs) form
    (let ((body (subst value (first args) exprs)))
      (if (eq 1 (length args))
          body
          (list kind (rest args) body)))))

(defun cps-transform-lookup (atom state)
  (cond
   ((eq state 'CL-USER::RETURN) (list state atom))
    ((cps-closure? state) (cps-uncurry state atom))
    (state (list state atom 'CL-USER::RETURN))
    (t (error 'cps-transform-error :form atom :state state))))

(defun cps-transform (form &optional state)
  (scan-list form #'cps-transform-lookup #'cps-transform-list
	     (or state 'CL-USER::RETURN)))
