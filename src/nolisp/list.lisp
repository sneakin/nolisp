;;;
;;; Utility functions
;;;

(in-package :nolisp)

(defun fix-improper-list (lst &optional acc)
  (cond
   ((not lst) (nreverse acc))
   ((atom lst) (list lst))
   ((not (rest lst)) (fix-improper-list nil (cons (first lst) acc)))
   ((atom (rest lst)) (fix-improper-list nil (cons (rest lst) (cons (first lst) acc))))
   (t (fix-improper-list (rest lst) (cons (first lst) acc)))))

(defun improper-mapcar (fn lst)
  (handler-case (mapcar fn lst)
    (type-error (e) (mapcar fn (fix-improper-list lst)))))

(defun flatten (lst)
  (cond
   ((atom lst) (list lst))
   ((not (rest lst)) lst)
   ((atom (rest lst)) (list (first lst) (rest lst)))
   (t (apply #'append (improper-mapcar #'flatten lst)))))

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

(defun reduce-values (fn lst &key key from-end (start 0) end initial-value)
  (if (and initial-value (atom initial-value))
      (reduce #'(lambda (acc i) (funcall fn i acc))
	      lst
	      :key key :from-end from-end :start start :end end
	      :initial-value initial-value)
    (values-list
     (reduce (if from-end
		 #'(lambda (i acc)
		     (multiple-value-list (apply fn (cons i acc))))
	       #'(lambda (acc i)
		   (multiple-value-list (apply fn (cons i acc)))))
	     lst
	     :key key :from-end from-end :start start :end end
	     :initial-value initial-value))))
