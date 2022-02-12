(in-package :nolisp)

(defun ifeq (a)
  "Returns a function that checks if it's argument is ~eq~ to ~a~."
  #'(lambda (x)
      (eq x a)))

(defmacro compose (&rest fns)
  "Returns a function that calls each function in ~fns~ successively with the output from the prior function.

Example: (compose #'equal #'not) => (lambda (a b) (not (equal a b)))"
  `(lambda (&rest args)
     ,(reduce #'(lambda (acc fn) `(funcall ,fn ,acc))
	      (rest fns)
	      :initial-value `(apply ,(first fns) args))))

(defun partial-first (fn &rest args)
  "Returns a function that is partially evaluated with ~args~ as the first arguments. Any arguments passed to the returned function appear after ~args~.

Example: (funcall (partial-first #'- 2 3) 4 5) => -10"
  #'(lambda (&rest more)
      (apply fn (append args more))))

(defun partial-after (fn &rest args)
  "Returns a function that is partially evaluated with ~args~ as the last arguments. An arguments passed to the returned function appear before ~args~.

Example: (funcall (partial-after #'- 2 3) 4 5) => -6"
  #'(lambda (&rest more)
      (apply fn (append more args))))
