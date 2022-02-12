(in-package :nolisp)

;;;
;;; The list scanner
;;;

(defun scan-list-reducer (lst visitor state)
  "A ~scan-list~ function for list's that uses ~reduce~ to call the visitor over a list."
  (reduce #'(lambda (state item) (funcall visitor item state))
	  lst :initial-value state))

(defun scan-list (form atom-visitor list-visitor
                  &optional
                  state
                  (recurser #'(lambda (f &optional (s state))
				(scan-list f atom-visitor list-visitor s))))
  "Iterate over a list calling the ~atom-visitor~ on atoms and ~list-visitor~ on sublists. The ~list-visitor~ gets passed a function to recurse deeper into sublists."
  (cond
    ((and form (listp form)) (funcall list-visitor form recurser state))
    (t (funcall atom-visitor form state))))
