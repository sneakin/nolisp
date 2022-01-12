;;;
;;; CPS Transform
;;;

(in-package :nolisp)

(define-condition cps-transform-error (nolisp-error) ())

(defun cps-atom? (form)
  (or (atom form)
      (eq 'CL-USER::LAMBDA (first form))
      (eq 'CL-USER::λ (first form))))

(defun cps-wrap (sym form cc)
  (cond
    ((or (null cc) (eq sym cc)) form)
    ((cps-atom? cc) `(,@form ,cc))
    (t `(,@form (CL-USER::λ (,sym) ,cc)))))

(defun cps-lambda (sym form)
  `(CL-USER::λ (,sym) ,form))

(defun lambda-form? (form)
  (and (listp form) (eq 'CL-USER::LAMBDA (first form))))

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

(defun cps-transform-call-inner (form visitor state &optional args fns (sym (gensym "R")))
  (if form
      (let ((tip (first form)))
        (if (cps-atom? tip)
            (cps-transform-call-inner (rest form) visitor state
                                         (cons (if (lambda-form? tip)
                                                   (funcall visitor tip)
                                                   tip)
                                               args)
                                         fns
                                         sym)
            (cps-transform-call-inner (rest form) visitor state
                                         (cons sym args)
                                         (acons sym tip fns)
                                         (gensym "R"))))
      (cps-transform-call-emit fns
                                  (cps-wrap sym (reverse args) state)
                                  visitor)))

(defun cps-transform-call (form visitor state)
  (cps-transform-call-inner form visitor state))

(defun cps-transform-list (form visitor state)
  (case (first form)
    (nil state)
    (IF
     (funcall visitor (second form)
              `(CL-USER::λ (cl-user::test)
                  (if cl-user::test
                      ,(funcall visitor (third form) state)
                      ,(funcall visitor (fourth form) state)))))
    (LAMBDA (let ((cc (gensym "CC"))
                  (fp (gensym "FP")))
              `(CL:LAMBDA (,cc ,fp ,@(second form))
                 ,(funcall visitor (if (fourth form)
                                       (rest (rest form))
                                       (third form))
                           cc))))
    (DEFUN (let ((cc (gensym "CC")))
             `(CL:DEFUN ,(second form) (,cc ,@(third form))
                ,(funcall visitor (if (fifth form)
                                      (rest (rest (rest form)))
                                      (fourth form))
                          cc))))
    (t (cps-transform-call form visitor state))))

(defun cps-uncurry (form value)
  (let ((body (subst value (first (second form)) (third form))))
    (if (eq 1 (length (second form)))
        body
        `(,(first form) ,(rest (second form))
           ,body))))

(defun cps-transform-lookup (atom state)
  (cond
    ((null state) atom)
    ((atom state) (list state atom))
    ((eq 'CL-USER::λ (first state)) (cps-uncurry state atom))
    (state (list state atom))
    (t (error 'cps-transform-error :form form :state state))))

(defun cps-transform (form &optional (state 'CL-USER::RETURN))
  (scan-list form #'cps-transform-lookup #'cps-transform-list state))
