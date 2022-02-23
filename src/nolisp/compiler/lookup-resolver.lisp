;;;
;;; Variable lookup in call frames
;;;

(in-package :nolisp)

(defun make-frame (args &optional parent env (depth 0))
  (list args parent env depth))
(defun frame-parent (frame)
  (second frame))
(defun frame-lookup-1 (frame sym)
  (position sym (first frame)))
(defun frame-closure (frame)
  (third frame))
(defun frame-closure-depth (frame)
  (fourth frame))
(defun frame-depth (frame &optional (n 0))
  (if (frame-parent frame)
      (frame-depth (frame-parent frame) (+ 1 n))
      n))

(defun frame-lookup (frame sym &optional (depth 0))
  (if frame
      (let ((index (frame-lookup-1 frame sym)))
        (if index
            (values index depth)
            (frame-lookup (frame-parent frame) sym (+ depth 1))))))

(defun frame-closure-lookup (frame sym &optional (cc-depth 0))
  (if frame
      (multiple-value-bind (index depth)
	  (frame-lookup frame sym)
        (if index
            (values cc-depth depth index)
            (frame-closure-lookup (frame-closure frame) sym (+ cc-depth 1))))))

(defun lookup-form? (form)
  (and (listp form)
       (or (eq 'CL-USER::ARGN (first form))
           (eq 'CL-USER::LOOKUP (first form))
	   (eq 'CL-USER::CLOSURE-LOOKUP (first form)))))

(defun lookup-resolver-atom (form state)
  (multiple-value-bind (index depth)
      (frame-lookup state form)
    (if index
        (if (eq depth 0)
            (list 'CL-USER::ARGN index)
            (list 'CL-USER::LOOKUP depth index))
      (multiple-value-bind (cc-depth depth index)
	  (frame-closure-lookup (frame-closure state) form)
	(if index
	    (list 'CL-USER::CLOSURE-LOOKUP
		  cc-depth (frame-closure-depth state) depth index)
	    form)))))

(defun lookup-resolver-call (visitor state args &optional ops)
  (if args
      (lookup-resolver-call visitor state
                            (rest args)
                            (cons (funcall (partial-after visitor state) (first args))
				  ops))
      (nreverse ops)))

(defun lookup-resolver-list (form visitor state)
  (match-case form
    ((CL-USER::LAMBDA ?args . ?body)
     (let ((closure (gensym "CC")))
       (multiple-value-bind
	(body cont) (clip-last body)
	`(CL-USER::LAMBDA (,closure ,@args)
			  ,@(mapcar (partial-after visitor
						   (make-frame (cons closure args)
							       nil
							       state
							       0))
				    body)
			  ,(funcall visitor cont state)))))
    ((CL-USER::λ ?args . ?body)
     `(CL-USER::λ ,args
		  ,@(mapcar (partial-after visitor
					   (make-frame args state
						       (frame-closure state)
						       (+ 1 (or (frame-closure-depth state) 0))))
			    body)))
    ((CL-USER::DEFUN ?name ?args . ?body)
     `(CL-USER::DEFUN ,name ,args
	,@(mapcar (partial-after visitor (make-frame args)) body)))
    (t (if (lookup-form? form)
           form
         (lookup-resolver-call visitor state form)))))

(defun lookup-resolver (form)
  (scan-list form
             #'lookup-resolver-atom
             #'lookup-resolver-list))
