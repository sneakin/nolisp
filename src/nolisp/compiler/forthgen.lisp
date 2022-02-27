;;
;; Forth walker
;;

(in-package :nolisp)

;;;
;;; State tracking and Forth code accumulator:
;;;

(defun make-forthgen-state (&optional code (depth 0))
  (list code depth))
(defun forthgen-state-code (state) (first state))
(defun forthgen-state-depth (state) (second state))

(defun forthgen-state-new-code (state code)
  (make-forthgen-state code
		       (forthgen-state-depth state)))

(defun forthgen-state-zero-depth (state)
  (make-forthgen-state (forthgen-state-code state) 0))
(defun forthgen-state-inc-depth (state &optional (amount 1))
  (make-forthgen-state (forthgen-state-code state)
		       (+ amount (forthgen-state-depth state))))

;;;
;;; Translated Form list
;;;

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

;;;
;;; scan-list visitor state updaters:
;;;

(defun forthgen-update-visitor (visitor new-state)
  #'(lambda (i) (funcall visitor i new-state)))

(defun forthgen-deeper-visitor (visitor state)
  (forthgen-update-visitor visitor (forthgen-state-inc-depth state)))

(defun forthgen-top-visitor (visitor state)
  (forthgen-update-visitor visitor (forthgen-state-zero-depth state)))

(defun forthgen-deeper-code-visitor (visitor state)
  (compose (forthgen-deeper-visitor visitor state)
	   #'forthgen-state-code))

;;;
;;; Form traversal
;;;

(defun forthgen-lookup? (form)
  (or (eq 'CL-USER::LOOKUP form)
      (eq 'CL-USER::ARGV form)
      (eq 'CL-USER::CLOSURE-LOOKUP form)))

(defun forthgen-emit-cc (form)
  (if (and (listp form)
	   (forthgen-lookup? (first (last form))))
      (list form 'CL-USER::EXEC 'CL-USER::EXIT-FRAME)
    form))

(defun forthgen-funcall (visitor state fn args &optional ops cont)
  (if args
      (let* ((new-state state)
             (newer-state (funcall visitor (first args) new-state))
	     (forth-form (forthgen-state-code newer-state)))
        (forthgen-funcall visitor new-state fn (rest args)
                          (if cont (cons forth-form ops) ops)
                          (if cont cont (forthgen-emit-cc forth-form))))
    `(,@ops ,fn ,cont)))

(defun forthgen-list (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *forth-forms*)))
    (forthgen-state-new-code
     state
     (cond
      (macro
       (apply (cdr macro) (cons visitor (cons state args))))
      ((listp name)
	(forthgen-funcall visitor state
			  'CL-USER::exec (shift-right (cons name args))))
      (t (forthgen-funcall visitor state
			   name (shift-right args)))))))

(defun forthgen-atom (sym state)
  (forthgen-state-new-code
   state
   (cond
    ((eq sym 'CL-USER::return)
     (if (< 0 (forthgen-state-depth state))
	 'CL-USER::exit-frame
	 :nonexit))
    ((keywordp sym) (list 'cl-user::keyword> sym))
    ((stringp sym) (concatenate 'string "\" " sym "\""))
    (t sym))))

(defun forthgen-scan (form &optional state)
  (scan-list form #'forthgen-atom #'forthgen-list
	     (or state (make-forthgen-state))))

(defun forthgen (form &optional state)
  (forthgen-state-code (forthgen-scan form state)))

;;;
;;; Translated forms
;;;

(define-forth-form IF (visitor state test then else)
  (declare (ignore state))
  (list (forthgen-state-code (funcall visitor test))
	'CL-USER::IF :newline
	(forthgen-state-code (funcall visitor then)) :newline
	'CL-USER::ELSE :newline
	(forthgen-state-code (funcall visitor else)) :newline
	'CL-USER::THEN))

(define-forth-form DEFUN (visitor state name args &rest body)
  (let ((inner-visitor (forthgen-deeper-code-visitor visitor state)))
    `(":" ,name "(" ,@(reverse args) ")" :newline
      CL-USER::begin-frame :newline
      ,@(mapcar inner-visitor body) :newline
      CL-USER::end-frame :newline ";")))

(define-forth-form λ (visitor state args &rest body)
  (let ((inner-visitor (forthgen-deeper-code-visitor visitor state)))
    `(:newline
      CL-USER::inner-frame "(" ,@args ")" :newline
      ,@(mapcar inner-visitor body) :newline
      CL-USER::end-frame)))

(define-forth-form LAMBDA (visitor state args &rest body)
  (let ((inner-visitor (compose (forthgen-top-visitor visitor state)
				#'forthgen-state-code)))
    (multiple-value-bind
     (body cc) (clip-last body)
     `("[" CL-USER::begin-frame  "(" ,@(reverse args) ")" :newline
       ,@(mapcar inner-visitor body) :newline
       CL-USER::end-frame :newline
       "]" CL-USER::close-lambda
       ,(forthgen-state-code (funcall visitor cc))))))

(define-forth-form PROGN (visitor state &rest calls)
  (declare (ignore state))
  (mapcar (compose visitor #'forthgen-state-code)
	  (nth-cons-from-end 2 calls)))

(define-forth-form RETURN (visitor state arg)
  (let ((code (forthgen-state-code (funcall visitor arg))))
    (if (< 0 (forthgen-state-depth state))
	(list code 'CL-USER::exit-frame)
      code)))

(define-forth-form ARGN (visitor state n)
  (declare (ignore visitor state))
  `(,n CL-USER::ARGN))

(define-forth-form LOOKUP (visitor state depth n)
  (declare (ignore visitor state))
  `(,n ,depth CL-USER::LOOKUP))

(define-forth-form CLOSURE-LOOKUP (visitor state cc-depth cc-links depth n)
  (declare (ignore visitor state))
  `(,n ,depth ,cc-links ,cc-depth CL-USER::CLOSURE-LOOKUP))

(define-forth-form QUOTE (visitor state sym cc)
  (declare (ignore state))
  `(CL-USER::QUOTE> ,sym ,(forthgen-state-code (funcall visitor cc))))

(define-forth-form FUNCTION (visitor state n cc)
  (declare (ignore state))
  ;; quote names
  (list "'" n (forthgen-state-code (funcall visitor cc))))

(define-forth-form defvar (visitor state name value cc)
  (declare (ignore state cc))
  `(,(forthgen-state-code (funcall visitor value)) CL-USER::VAR> ,name))

(define-forth-form defconstant (visitor state name value cc)
  (declare (ignore state cc))
  `(,(forthgen-state-code (funcall visitor value)) CL-USER::CONST> ,name))

;;;
;;; Forms where arguments need reversing.
;;;

(defun forthgen-arg-reverser (op)
  #'(lambda (visitor state &rest args)
      (declare (ignore state))
      (multiple-value-bind (lst last-item) (clip-last args)
	(mapcar (compose visitor #'forthgen-state-code)
		(append lst (list op last-item))))))

(mapcar (compose
	 #'(lambda (op) (intern (symbol-name op) :cl-user))
	 #'(lambda (op) (update-forth-form op (forthgen-arg-reverser op))))
	'(- / < <= >= >))
