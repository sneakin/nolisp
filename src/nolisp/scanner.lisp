(in-package :nolisp)

;;;
;;; The list scanner
;;;

;; A ~scan-list~ function for list's that uses ~reduce~ to call the visitor
;; over a list.
(defun scan-list-reducer (lst visitor state)
  (reduce #'(lambda (state item) (funcall visitor item state))
	  lst :initial-value state))

;; Iterate over a list calling the ~atom-visitor~ on atoms and ~list-visitor~
;; on sublists. The ~list-visitor~ gets passed a function to recurse deeper into
;; sublists.
(defun scan-list (form atom-visitor list-visitor
                  &optional
                    state
                    (recurser #'(lambda (f &optional (s state))
				  (scan-list f atom-visitor list-visitor s))))
  (cond
    ((and form (listp form)) (funcall list-visitor form recurser state))
    (t (funcall atom-visitor form state))))
