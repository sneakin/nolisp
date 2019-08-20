(require "nolisp/error")
(require "nolisp/scanner")

;;;
;;; CPS Transform
;;;

(define-condition nc-cps-transform-error (nc-error) ())

(defun cps-atom? (form)
  (or (atom form)
      (eq 'LAMBDA (first form))
      (eq 'λ (first form))))

(defun cps-wrap (sym form cc)
  (cond
    ((or (null cc) (eq sym cc)) form)
    ((cps-atom? cc) `(,@form ,cc))
    (t `(,@form (λ (,sym) ,cc)))))

(defun cps-lambda (sym form)
  `(λ (,sym) ,form))

(defun lambda-form? (form)
  (and (listp form) (eq 'LAMBDA (first form))))

(defun nc-cps-transform-call-emit (fns ops visitor &optional (first-call t))
  (if fns
      (let* ((form (rest (first fns)))
             (sym (first (first fns)))
             (new-form (funcall visitor form (cps-lambda sym ops))))
        (nc-cps-transform-call-emit (rest fns)
                                    new-form
                                    visitor
                                    nil))
      ops))

(defun nc-cps-transform-call-inner (form visitor state &optional args fns (sym (gensym "R")))
  (if form
      (let ((tip (first form)))
        (if (cps-atom? tip)
            (nc-cps-transform-call-inner (rest form) visitor state
                                         (cons (if (lambda-form? tip)
                                                   (funcall visitor tip)
                                                   tip)
                                               args)
                                         fns
                                         sym)
            (nc-cps-transform-call-inner (rest form) visitor state
                                         (cons sym args)
                                         (acons sym tip fns)
                                         (gensym "R"))))
      (nc-cps-transform-call-emit fns
                                  (cps-wrap sym (reverse args) state)
                                  visitor)))

(defun nc-cps-transform-call (form visitor state)
  (nc-cps-transform-call-inner form visitor state))

(defun nc-cps-transform-list (form visitor state)
  (case (first form)
    (nil state)
    (IF
     (funcall visitor (second form)
              `(λ (test)
                  (if test
                      ,(funcall visitor (third form) state)
                      ,(funcall visitor (fourth form) state)))))
    (LAMBDA (let ((cc (gensym "CC"))
                  (fp (gensym "FP")))
              `(LAMBDA (,cc ,fp ,@(second form))
                 ,(funcall visitor (if (fourth form)
                                       (rest (rest form))
                                       (third form))
                           cc))))
    (DEFUN (let ((cc (gensym "CC")))
             `(DEFUN ,(second form) (,cc ,@(third form))
                ,(funcall visitor (if (fifth form)
                                      (rest (rest (rest form)))
                                      (fourth form))
                          cc))))
    (t (nc-cps-transform-call form visitor state))))

(defun nc-cps-uncurry (form value)
  (let ((body (subst value (first (second form)) (third form))))
    (if (eq 1 (length (second form)))
        body
        `(,(first form) ,(rest (second form))
           ,body))))

(defun nc-cps-transform-lookup (atom state)
  (cond
    ((null state) atom)
    ((atom state) (list state atom))
    ((eq 'λ (first state)) (nc-cps-uncurry state atom))
    (state (list state atom))
    (t (error 'nc-cps-transform-error :form form :state state))))

(defun nc-cps-transform (form &optional (state 'RETURN))
  (scan-list form #'nc-cps-transform-lookup #'nc-cps-transform-list state))
