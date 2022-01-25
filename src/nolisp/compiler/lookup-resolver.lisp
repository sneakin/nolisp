;;;
;;; Variable lookup in call frames
;;;

(in-package :nolisp)

(defun make-frame (args &optional parent env)
  (list args parent env))
(defun frame-parent (frame)
  (second frame))
(defun frame-lookup-1 (frame sym)
  (position sym (first frame)))
(defun frame-closure (frame)
  (third frame))

(defun frame-lookup (frame sym &optional (depth 0))
  (if frame
      (let ((index (frame-lookup-1 frame sym)))
        (if index
            (values index depth)
            (frame-lookup (frame-parent frame) sym (+ depth 1))))))

(defun lookup-form? (form)
  (and (listp form)
       (or (eq 'CL-USER::ARGN (first form))
           (eq 'CL-USER::LOOKUP (first form)))))

(defun lookup-resolver-atom (form state &optional (depth 0))
  (multiple-value-bind (index depth)
      (frame-lookup state form)
    (if index
        (if (eq depth 0)
            (list 'CL-USER::ARGN index)
            (list 'CL-USER::LOOKUP depth index))
      (multiple-value-bind (index depth)
	  (frame-lookup (frame-closure state) form)
	(if index
	    (list 'CL-USER::CLOSURE-LOOKUP depth index)
	  form)))))

(defun lookup-resolver-call (visitor state args &optional ops)
  (if args
      (lookup-resolver-call visitor state
                            (rest args)
                            (cons (funcall (partial-after visitor state) (first args)) ops))
      (nreverse ops)))

(defun lookup-resolver-list (form visitor state)
  (case (first form)
    (CL-USER::LAMBDA
     (let ((args (second form))
	   (closure (gensym "CC"))
	   (body (rest (rest form))))
       `(CL-USER::LAMBDA (,closure ,@args)
			 ,@(mapcar (partial-after visitor
						  (make-frame (cons closure args)
							      nil
							      state))
				   body))))
    (CL-USER::λ
     (let ((args (second form))
	   (body (rest (rest form))))
       `(CL-USER::λ ,args
		    ,@(mapcar (partial-after visitor (make-frame args state))
			      body))))
    (CL-USER::DEFUN
     (let ((name (second form))
	   (args (third form))
	   (body (rest (rest (rest form)))))
       `(CL-USER::DEFUN ,name ,args
			,@(mapcar (partial-after visitor (make-frame args)) body))))
    (t (if (lookup-form? form)
           form
         (lookup-resolver-call visitor state form)))))

(defun lookup-resolver (form)
  (scan-list form
             #'lookup-resolver-atom
             #'lookup-resolver-list))
