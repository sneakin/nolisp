(require "nolisp/scanner")

;;;
;;; Macroexpand
;;;

(defvar *macros* '())

(defun macro? (form)
  (not (not (assoc form *macros*))))

(defun nc-remove-macro (name)
  (let ((macro (assoc name *macros*)))
    (if macro
        (setq *macros* (remove macro *macros*)))))

(defun nc-add-macro (name fn)
  (push (cons name fn) *macros*)
  fn)

(defun nc-update-macro (name fn)
  (nc-remove-macro name)
  (nc-add-macro name fn))

(defun gen-macro-caller (args body)
  (eval `(lambda ,args ,@body)))

(defun nc-defmacro (form env)
  (nc-update-macro (first form) (gen-macro-caller (second form) (rest (rest form))))
  nil)

(defun nc-call-macro (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *macros*)))
    (if macro
        (apply (cdr macro) args)
        (mapcar visitor form))))

(defun nc-macroexpand-1 (form)
  (scan-list form
             #'(lambda (a state) a)
             #'nc-call-macro))

(defun nc-macroexpand (form)
  (let ((new-form (nc-macroexpand-1 form)))
    (if (equal form new-form)
        form
        (nc-macroexpand new-form))))

(nc-update-macro 'LET #'(lambda (bindings &rest body)
                          `(funcall (lambda ,(mapcar #'first bindings)
                                      ,@body)
                                    ,@(mapcar #'second bindings))))
