(defun ifeq (a)
  #'(lambda (x)
      (eq x a)))

(defun curry (fn &rest args)
  #'(lambda (&rest more)
      (apply fn (append args more))))

(defun curry-after (fn &rest args)
  #'(lambda (&rest more)
      (apply fn (append more args))))
