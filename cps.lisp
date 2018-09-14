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

(defun to-cps-atom (cell cc)
  (funcall cc cell))

(defun lambda? (cell)
  (and (listp cell)
       (or (equal (symbol-name (first cell)) "λ") ; eq λ wasn't working
           (eq (first cell) *FN*)
           (eq (first cell) 'lambda))))

(defun to-cps-value (cell)
  (cond
    ((and (not (atom cell))
          (lambda? cell))
     (let ((vars (second cell))
           (expr (rest (rest cell)))
           (cc-sym (gensym)))
       `(fn (,@vars ,cc-sym) ,(to-cps-list expr (lambda (r)
                                                 `(call ,cc-sym ,@r))))))
    (t cell)))

(defun to-cps-list (cell cc)
  (cond
    ((null cell)
     (funcall cc nil))
    ((listp cell)
     (to-cps (first cell)
             (lambda (f)
               (to-cps-list (rest cell)
                            (lambda (g)
                              (funcall cc (cons f g)))))))))

(defun to-cps (cell &optional (cc #'identity) args)
  (cond
    ((or (atom cell) (lambda? cell))
     (funcall cc (to-cps-value cell)))
    ((eq (first cell) *IF*)
     (to-cps (second cell)
             (lambda (cr)
               `(if ,cr
                    ,`(fn () ,(to-cps (third cell) (lambda (rr) (funcall cc rr)) args))
                    ,`(fn () ,(to-cps (fourth cell) (lambda (rr) (funcall cc rr)) args))))))
    ((eq (first cell) *MVB*)
     (to-cps (third cell)
             (lambda (r)
               (declare (ignorable r))
               (to-cps (fourth cell) (lambda (rr) (funcall cc rr))))
             (second cell)))
    (t (let* ((cc-sym (gensym))
              (cont `(fn ,(if args args (list cc-sym))
                         ,(funcall cc cc-sym))))
         (to-cps (first cell)
                 (lambda (f)
                   (to-cps-list (rest cell)
                                (lambda (e)
                                  `(call ,f ,@e ,cont))))
                 args)))))
