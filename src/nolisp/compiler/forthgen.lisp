;;
;; Forth walker
;;

(in-package :nolisp)

(defvar *forth-forms* '())

(defun add-forth-form (name fn)
  (push (cons name fn) *forth-forms*)
  fn)

(defun remove-forth-form (name)
  (let ((macro (assoc name *forth-forms*)))
    (if macro
        (setq *forth-forms* (remove macro *forth-forms*)))))

(defun update-forth-form (name fn)
  (remove-forth-form name)
  (add-forth-form name fn))

(defmacro define-forth-form (name arglist &rest body)
  `(update-forth-form ',(intern (symbol-name name) :cl-user)
		      #'(lambda ,arglist ,@body)))

(defun forthgen-arg-loaders (args &optional ops (offset 0))
  (if args
      (let ((item (first args)))
        (if (atom item)
            (forthgen-arg-loaders (rest args)
                                  (cons (first args) ops)
                                  (+ offset 1))
            (let* ((new-offset (+ offset 1 (if (atom item) 1 (length item)))))
              (forthgen-arg-loaders (rest args)
                                    (if (> offset 0)
                                        (cons `(,offset overn) ops)
                                      ops)
                                    new-offset))))
      
      (nreverse ops)))

(defun forthgen-funcall (visitor state fn args &optional ops cont)
  (if args
      (let* ((new-state state)
             (new-visitor (partial-after visitor new-state))
             (forth-form (funcall new-visitor (first args))))
        (forthgen-funcall visitor new-state fn (rest args)
                             (if cont (cons forth-form ops) ops)
                             (if cont cont forth-form)))
      `(,@ops ,fn ,cont)))

(defun forthgen-list (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *forth-forms*)))
    (if macro
        (apply (cdr macro) (cons visitor args))
        (forthgen-funcall visitor state name (shift-right args)))))

(defun forthgen-lookup (sym state)
  (cond
   ((eq sym 'CL-USER::return) 'CL-USER::exit-frame)
   (t (identity sym))))

(defun forthgen (form)
  (scan-list form
             #'forthgen-lookup
             #'forthgen-list))

(define-forth-form IF (visitor test then else)
  `(,(funcall visitor test) CL-USER::IF :newline
    ,(funcall visitor then) CL-USER::ELSE :newline
    ,(funcall visitor else) CL-USER::THEN :newline))

(define-forth-form DEFUN (visitor name args &rest body)
  `(":" ,name "(" ,@(reverse args) ")" :newline
    CL-USER::begin-frame :newline
    ,@(mapcar visitor body)
    CL-USER::end-frame :newline ";"))

(define-forth-form λ (visitor args &rest body)
  `(:newline
    CL-USER::begin-frame "(" ,@args ")" :newline
    ,@(mapcar visitor body)
    CL-USER::end-frame :newline))

(define-forth-form LAMBDA (visitor args &rest body)
  `("[" CL-USER::begin-frame  "(" ,@args ")" :newline
    ,@(mapcar visitor body)
    CL-USER::frame-return :newline
    CL-USER::end-frame :newline
    "]" CL-USER::close-lambda))

(define-forth-form PROGN (visitor &rest calls)
  (mapcar visitor calls))

(define-forth-form RETURN (visitor arg)
  (list (funcall visitor arg)
	'CL-USER::exit-frame))

(define-forth-form ARGN (visitor n)
  `(,n CL-USER::ARGN))

(define-forth-form LOOKUP (visitor depth n)
  `(,n ,depth CL-USER::LOOKUP))

(defun forthgen-arg-reverser (op)
  #'(lambda (visitor &rest args)
      (multiple-value-bind (lst last-item)
          (clip-last args)
        (mapcar visitor (append lst (list op last-item))))))

(mapcar (compose
	 #'(lambda (op) (intern (symbol-name op) :cl-user))
	 #'(lambda (op) (update-forth-form op (forthgen-arg-reverser op))))
	'(+ - * / < <= >= > ^ **))
