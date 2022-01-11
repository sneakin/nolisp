;;
;; Forth walker
;;

(in-package :nolisp)

(defvar *forth-forms* '())

(defun nc-add-forth-form (name fn)
  (push (cons name fn) *forth-forms*)
  fn)

(defun nc-remove-forth-form (name)
  (let ((macro (assoc name *forth-forms*)))
    (if macro
        (setq *forth-forms* (remove macro *forth-forms*)))))

(defun nc-update-forth-form (name fn)
  (nc-remove-forth-form name)
  (nc-add-forth-form name fn))

(defun nc-forthgen-arg-loaders (args &optional ops (offset 0))
  (if args
      (let ((item (first args)))
        (if (atom item)
            (nc-forthgen-arg-loaders (rest args)
                                     (cons (first args) ops)
                                     (+ offset 1))
            (let* ((new-offset (+ offset 1 (if (atom item) 1 (length item)))))
              (nc-forthgen-arg-loaders (rest args)
                                       (if (> offset 0)
                                           (cons `(,offset overn) ops)
                                           ops)
                                       new-offset))))
      
      (nreverse ops)))

(defun nc-forthgen-funcall (visitor state fn args &optional ops cont)
  (if args
      (let* ((new-state state)
             (new-visitor (partial-after visitor new-state))
             (forth-form (funcall new-visitor (first args))))
        (nc-forthgen-funcall visitor new-state fn (rest args)
                             (if cont (cons forth-form ops) ops)
                             (if cont cont forth-form)))
      `(,@ops ,fn ,cont)))

(defun nc-forthgen-list (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *forth-forms*)))
    (if macro
        (apply (cdr macro) (cons visitor args))
        (nc-forthgen-funcall visitor state name (shift-right args)))))

(defun nc-forthgen-lookup (sym state)
  (identity sym))

(defun nc-forthgen (form)
  (scan-list form
             #'nc-forthgen-lookup
             #'nc-forthgen-list))

(nc-update-forth-form 'CL-USER::IF #'(lambda (visitor test then else)
                              `(,(funcall visitor test) CL-USER::IF :newline
                                 ,(funcall visitor then) CL-USER::ELSE :newline
                                 ,(funcall visitor else) CL-USER::THEN :newline)))
(nc-update-forth-form 'CL-USER::DEFUN #'(lambda (visitor name args &rest body)
                                 `(":" ,name "(" CL-USER::CC ,@(rest args) ")" :newline
                                       CL-USER::begin-frame :newline
                                       ,@(mapcar visitor body)
                                       CL-USER::frame-return :newline ";")))
(nc-update-forth-form 'CL-USER::λ #'(lambda (visitor args &rest body)
                             `("(" ,@args ")" :newline
                                   ,@(mapcar visitor body) :newline
                                   CL-USER::tail-frame
                                   )))
(nc-update-forth-form 'CL-USER::LAMBDA #'(lambda (visitor args &rest body)
                                  `("[" CL-USER::begin-frame  "(" CL-USER::CC CL-USER::FP ,@(rest (rest args)) ")" :newline
                                        ,@(mapcar visitor body)
                                        CL-USER::frame-return :newline
                                        "]" CL-USER::close-lambda :newline)))
(nc-update-forth-form 'CL-USER::PROGN #'(lambda (visitor &rest calls)
                                 (mapcar visitor calls)))
(nc-update-forth-form 'CL-USER::RETURN #'(lambda (visitor arg)
                                  (funcall visitor arg)))
(nc-update-forth-form 'CL-USER::ARGN #'(lambda (visitor n)
                                `(CL-USER::ARGN> ,n)))
(nc-update-forth-form 'CL-USER::LOOKUP #'(lambda (visitor depth n)
                                  `(CL-USER::LOOKUP> ,depth ,n)))

(defun nc-forthgen-arg-reverser (op)
  #'(lambda (visitor &rest args)
      (multiple-value-bind (lst last-item)
          (clip-last args)
        (mapcar visitor (append lst (list op last-item))))))

(mapcar (compose
	 #'(lambda (op) (intern (symbol-name op) :cl-user))
	 #'(lambda (op) (nc-update-forth-form op (nc-forthgen-arg-reverser op))))
	'(+ - * / < <= >= > ^ **))

