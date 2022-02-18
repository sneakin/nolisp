(in-package :nolisp)

(defun assoc-get (key lst)
  (rest (assoc key lst)))

(defun assoc-bind-gen (keys lst &key (key-namer #'identity) body)
  `(destructuring-bind ,(mapcar key-namer keys)
       (mapcar #'(lambda (k) (assoc-get k ,lst)) ',keys)
     ,@body))

(defmacro assoc-bind (keys lst &rest body)
  (assoc-bind-gen keys lst :body body))

