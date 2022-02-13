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
  (cond
   ((or (null cc) (eq sym cc)) form)
   ((cps-atom? cc) `(,@form ,cc))
   (t `(,@form ,(cps-lambda sym cc)))))

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

(defun cps-emit-if (form visitor state cc)
  (funcall visitor (second form)
	   `(CL-USER::λ (cl-user::test)
			(if cl-user::test
			    ,(funcall visitor (third form) cc)
			  ,(funcall visitor (fourth form) cc)))))

(defun cps-transform-list (form visitor state)
  (case (first form)
    (nil state)
    (IF
     ;; transform IF statements so any caller is placed inside
     ;; a new lambda used as the continuation of the two clauses
     (if (or (eq state nil) (eq state 'CL-USER::RETURN))
	 (cps-emit-if form visitor state state)
       (let ((cc (gensym "CC")))
         `(CL:LAMBDA ,(second state)
		     ,@(cddr state)
		     (CL-USER::λ (,cc) ,(cps-emit-if form visitor state cc))))))
    (LAMBDA `(CL:LAMBDA ,(second form)
			,(funcall visitor
				  (if (fourth form)
				      (rest (rest form))
				    (third form))
				  'CL-USER::RETURN)
			,state))
    (DEFUN `(CL:DEFUN ,(second form) ,(third form)
                      ,(funcall visitor (if (fifth form)
					    (rest (rest (rest form)))
					  (fourth form)))))
    (t (cps-transform-call form visitor state))))

(defun cps-uncurry (form value)
  (let ((body (subst value (first (second form)) (third form))))
    (if (eq 1 (length (second form)))
        body
        (list (first form) (rest (second form)) body))))

(defun cps-transform-lookup (atom state)
  (cond
   ((eq state 'CL-USER::RETURN) (list state atom))
    ((cps-closure? state) (cps-uncurry state atom))
    (state (list state atom 'CL-USER::RETURN))
    (t (error 'cps-transform-error :form atom :state state))))

(defun cps-transform (form &optional state)
  (scan-list form #'cps-transform-lookup #'cps-transform-list
	     (or state 'CL-USER::RETURN)))
