(in-package :nolisp)

(defun ifeq (a)
  #'(lambda (x)
      (eq x a)))

(defmacro compose (&rest fns)
  `(lambda (&rest args)
     ,(reduce #'(lambda (acc fn) `(funcall ,fn ,acc))
	      (rest fns)
	      :initial-value `(apply ,(first fns) args))))

(defun partial-first (fn &rest args)
  #'(lambda (&rest more)
      (apply fn (append args more))))

(defun partial-after (fn &rest args)
  #'(lambda (&rest more)
      (apply fn (append more args))))
