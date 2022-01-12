;;;
;;; Macroexpand
;;;

(in-package :nolisp)

(defvar *macros* '())

(defun macro? (form)
  (not (not (assoc form *macros*))))

(defun remove-macro (name)
  (let ((macro (assoc name *macros*)))
    (if macro
        (setq *macros* (remove macro *macros*)))))

(defun add-macro (name fn)
  (push (cons name fn) *macros*)
  fn)

(defun update-macro (name fn)
  (remove-macro name)
  (add-macro name fn))

(defun gen-macro-caller (args body)
  (eval `(lambda ,args ,@body)))

(defun define-macro (form env)
  (update-macro (first form) (gen-macro-caller (second form) (rest (rest form))))
  nil)

(defun call-macro (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *macros*)))
    (if macro
        (apply (cdr macro) args)
        (mapcar visitor form))))

(defun macro-expand-1 (form)
  (scan-list form
             #'(lambda (a state) a)
             #'call-macro))

(defun macro-expand (form)
  (let ((new-form (macro-expand-1 form)))
    (if (equal form new-form)
        form
        (macro-expand new-form))))

(update-macro 'LET #'(lambda (bindings &rest body)
                          `(funcall (lambda ,(mapcar #'first bindings)
                                      ,@body)
                                    ,@(mapcar #'second bindings))))
