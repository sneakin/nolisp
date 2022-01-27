;;;
;;; Utility functions
;;;

(in-package :nolisp)

(defun flatten (lst &optional result (top t))
  (if lst
      (if (atom lst)
          (cons lst result)
        (flatten (rest lst)
		 (if (first lst)
		     (flatten (first lst) result nil)
		   (cons (first lst) result))
		 top))
    (if top
        (nreverse result)
      result)))

(defun clip-last (lst)
  (let ((rl (reverse lst)))
    (values (nreverse (rest rl))
            (first rl))))

(defun nshift-left (lst)
  (rplacd (last lst) (cons (first lst) nil))
  (rest lst))

(defun shift-left (lst)
  (append (rest lst) (list (first lst))))

(defun shift-right (lst)
  (nreverse (nolisp:shift-left (reverse lst))))
