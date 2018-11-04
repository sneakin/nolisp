;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;;
;;; Functions to convert expressions to continuation passing style. The only recognized
;;; forms are atoms, lambda, IF expressions, and calls.
;;; http://matt.might.net/articles/cps-conversion/ was of much help.

(in-package :repl)

;;; ensure these symbols get interned outside of the package
(defconstant *LB* 'λ)
(defconstant *FN* (intern "FN" :cl-user))
(defconstant *IF* (intern "IF" :cl-user))
(defconstant *MVB* (intern "MVB" :cl-user))

(defun lambda? (cell)
  (and (listp cell)
       (or (equal (symbol-name (first cell)) "λ") ; eq λ wasn't working
           (eq (first cell) *FN*)
           (eq (first cell) 'lambda))))

(defun to-cps-value (cell captures)
  (cond
    ((and (not (atom cell))
          (lambda? cell))
     (let ((vars (second cell))
           (expr (rest (rest cell)))
           (cc-sym (gensym)))
       (multiple-value-bind (captures expr)
           (to-cps-list expr
                        captures
                        (lambda (captures r)
                          (values captures `(call ,cc-sym ,@r ,captures))))
         (values captures `(fn (,@vars ,cc-sym) ,captures ,expr)))))
    (t (values captures cell))))

(defun to-cps-list (cell captures cc)
  (cond
    ((null cell)
     (funcall cc captures nil))
    ((listp cell)
     (to-cps (first cell)
             captures
             (lambda (f-caps f)
               (to-cps-list (rest cell)
                            f-caps
                            (lambda (g-caps g)
                              (funcall cc g-caps (cons f g)))))))))



(defun to-cps (cell &optional captures (cc #'values) args)
  (cond
    ((null cell)
     (funcall cc captures nil))
    ((or (atom cell) (lambda? cell))
     (multiple-value-bind (captures expr)
         (to-cps-value cell captures)
       (funcall cc
                (if (symbolp cell)
                    (cons cell captures)
                    captures)
                expr)))
    ((eq (first cell) *IF*)
     (to-cps (second cell) captures
             (lambda (test-captures cr)
               (multiple-value-bind (then-captures then-expr)
                   (to-cps (third cell)
                           test-captures
                           (lambda (captures rr)
                             (funcall cc captures rr))
                           args)
                 (multiple-value-bind (else-captures else-expr)
                     (to-cps (fourth cell)
                             test-captures
                             (lambda (captures rr)
                               (funcall cc captures rr))
                             args)
                   (values (remove-duplicates (append else-captures then-captures))
                           `(if ,cr ,test-captures
                                ,`(fn () ,then-captures ,then-expr)
                                ,`(fn () ,else-captures ,else-expr))))))))
    ((eq (first cell) *MVB*)
     (to-cps (third cell)
             captures
             (lambda (captures r)
               (declare (ignorable r))
               (to-cps (fourth cell)
                       captures
                       (lambda (captures rr)
                         (funcall cc captures rr))))
             (second cell)))
    (t (to-cps (first cell)
               captures
               (lambda (captures f)
                 (to-cps-list (rest cell)
                              captures
                              (lambda (captures e)
                                (let* ((cc-sym (gensym))           
                                       (cont (multiple-value-bind (captures expr)
                                                 (funcall cc captures cc-sym)
                                               `(fn ,(if args (append args (list cc-sym)) (list cc-sym))
                                                    ,captures
                                                    ,expr))))
                                  (values captures `(call ,f ,e ,captures ,cont)))
                                )))
               args))))
