(require "nolisp/fun")

;;;
;;; Variable lookup in call frames
;;;

(defun make-frame (args &optional parent)
  (cons args parent))
(defun frame-parent (frame)
  (cdr frame))
(defun frame-lookup-1 (frame sym)
  (position sym (first frame)))

(defun frame-lookup (frame sym &optional (depth 0))
  (if frame
      (let ((index (frame-lookup-1 frame sym)))
        (if index
            (values index depth)
            (frame-lookup (frame-parent frame) sym (+ depth 1))))
      ))

(defun nc-lookup-form? (form)
  (and (listp form)
       (or (eq 'ARGN (first form))
           (eq 'LOOKUP (first form)))))

(defun nc-lookup-resolver-atom (form state &optional (depth 0))
  (multiple-value-bind (index depth)
      (frame-lookup state form)
    (if index
        (if (eq depth 0)
            (list 'ARGN index)
            (list 'LOOKUP depth index))
        form)))

(defun nc-lookup-resolver-call (visitor state name args &optional ops)
  (if args
      (nc-lookup-resolver-call visitor state
                             name (rest args)
                             (cons (funcall (curry-after visitor state) (first args)) ops))
      (nreverse ops)))

(defun nc-lookup-resolver-list (form visitor state)
  (case (first form)
    (LAMBDA (let ((args (second form))
                  (body (rest (rest form))))
              `(LAMBDA ,args
                 ,@(mapcar (curry-after visitor (make-frame args state))
                           body))))
    (λ (let ((args (second form))
             (body (rest (rest form))))
         `(λ ,args
             ,@(mapcar (curry-after visitor (make-frame args state))
                       body))))
    (DEFUN (let ((name (second form))
                 (args (third form))
                 (body (rest (rest (rest form)))))
             `(DEFUN ,name ,args
                ,@(mapcar (curry-after visitor (make-frame args)) body))))
    (t (if (nc-lookup-form? form)
           form
           (cons (first form)
                 (nc-lookup-resolver-call visitor state (first form) (rest form))
                 )))))

(defun nc-lookup-resolver (form)
  (scan-list form
             #'nc-lookup-resolver-atom
             #'nc-lookup-resolver-list))
