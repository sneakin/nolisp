;;;
;;; Variable lookup in call frames
;;;

(in-package :nolisp)

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

(defun lookup-form? (form)
  (and (listp form)
       (or (eq 'CL-USER::ARGN (first form))
           (eq 'CL-USER::LOOKUP (first form)))))

(defun lookup-resolver-atom (form state &optional (depth 0))
  (multiple-value-bind (index depth)
      (frame-lookup state form)
    (if index
        (if (eq depth 0)
            (list 'CL-USER::ARGN index)
            (list 'CL-USER::LOOKUP depth index))
        form)))

(defun lookup-resolver-call (visitor state name args &optional ops)
  (if args
      (lookup-resolver-call visitor state
                             name (rest args)
                             (cons (funcall (partial-after visitor state) (first args)) ops))
      (nreverse ops)))

(defun lookup-resolver-list (form visitor state)
  (case (first form)
    (LAMBDA (let ((args (second form))
                  (body (rest (rest form))))
              `(LAMBDA ,args
                 ,@(mapcar (partial-after visitor (make-frame args state))
                           body))))
    (CL-USER::λ (let ((args (second form))
             (body (rest (rest form))))
         `(CL-USER::λ ,args
             ,@(mapcar (partial-after visitor (make-frame args state))
                       body))))
    (DEFUN (let ((name (second form))
                 (args (third form))
                 (body (rest (rest (rest form)))))
             `(DEFUN ,name ,args
                ,@(mapcar (partial-after visitor (make-frame args)) body))))
    (t (if (lookup-form? form)
           form
           (cons (first form)
                 (lookup-resolver-call visitor state (first form) (rest form))
                 )))))

(defun lookup-resolver (form)
  (scan-list form
             #'lookup-resolver-atom
             #'lookup-resolver-list))
