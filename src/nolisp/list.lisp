;;;
;;; Utility functions
;;;

(in-package :nolisp)

(defun any? (lst)
  (not (null (reduce #'(lambda (a b) (or a b)) lst))))

(defun all? (lst)
  (not (null (reduce #'(lambda (a b) (and a b)) lst))))

(defun last? (cell)
  (atom (rest cell)))

(defun improper-cell? (cell)
  (and (rest cell) (last? cell)))

(defun fix-improper-list-loop (lst &optional acc)
  (cond
   ((null lst) (nreverse acc))
   ((null (rest lst)) (fix-improper-list-loop nil (cons (first lst) acc)))
   ((last? lst) (fix-improper-list-loop nil (list* (rest lst) (first lst) acc)))
   (t (fix-improper-list-loop (rest lst) (cons (first lst) acc)))))

(defun fix-improper-list (lst)
  (cond
   ((null lst) lst)
   ((atom lst) (list lst))
   ((rest (last lst)) (fix-improper-list-loop lst))
   (t lst)))

(defun fix-improper-list! (lst)
  (if (atom lst)
      (list lst)
    (let ((l (last lst)))
      (if (rest l) (rplacd l (list (rest l))))
      lst)))

;;; stab at multiple lists
(defun mapl-improper-loop (acc fn lsts)
  (cond
    ((null (first lsts)) (nreverse acc))
    ;;((atom lst) (funcall fn lst)) ;; todo always call fn w/ cell?
    ((improper-cell? (first lsts)) (append (nreverse acc) (apply fn lsts)))
    (t (mapl-improper-loop (cons (apply fn (mapcar #'first lsts)) acc)
			   fn (mapcar #'rest lsts)))))

(defun mapl-improper (fn lst &optional acc)
  "Maps ~fn~ over each cell of ~lst~ that may be improper. The returned list has the same structure."
  (cond
    ((null lst) (nreverse acc))
    ;;((atom lst) (funcall fn lst)) ;; todo always call fn w/ cell?
    ((improper-cell? lst) (append (nreverse acc) (funcall fn lst)))
    (t (mapl-improper fn (rest lst) (cons (funcall fn lst) acc)))))

(defun mapl-improper-fixed (fn lst &optional acc)
  "Maps ~fn~ over each cell of ~lst~ that may be improper. The returned list is always a proper list."
  (cond
    ((null lst) (nreverse acc))
    ((improper-cell? lst) (mapl-improper-fixed fn nil (cons (funcall fn lst) acc)))
    (t (mapl-improper-fixed fn (rest lst) (cons (funcall fn lst) acc)))))

(defun improper-builder (fn build-fn)
  #'(lambda (el)
      (cond
	((improper-cell? el)
	 (funcall build-fn (funcall fn (first el)) (funcall fn (rest el))))
	(t (funcall fn (first el))))))

(defun mapelt-improper (fn lst)
  "Map ~fn~ over each element of ~lst~. This separates the tail from an improper list while maintaining the structure ot return."
  (mapl-improper (improper-builder fn #'cons) lst))

(defun mapelt-improper-fixed (fn lst)
  "Map ~fn~ over each element of ~lst~. This separates the tail from an improper list while returning a proper list."
  (mapl-improper (improper-builder fn #'list) lst))

;; the original that may get removed along w/ improper-mapcar
(defun map-improper (fn lst &optional acc)
  "Map ~fn~ over each element of ~lst~. This separates the tail from an improper list while maintaining the structure ot return."
  (cond
    ((null lst) (nreverse acc))
    ((atom lst) (funcall fn lst))
    ((improper-cell? lst)
     ;; todo decide if cells should be iterated in parts.
     (append (nreverse acc) (cons (funcall fn (first lst)) (funcall fn (rest lst)))))
     ;;(append (nreverse acc) (funcall fn lst)))
    (t (map-improper fn (rest lst) (cons (funcall fn (first lst)) acc)))))

;; (defun fix-improper-list (lst)
;;   (if (atom lst)
;;       (list lst)
;;       (mapelt-improper-fixed #'identity lst)))

(defun improper-mapcar (fn lst)
  ;;(mapcar fn (fix-improper-list lst))
  (mapelt-improper-fixed fn lst))

(defun flatten (lst)
  (cond
   ((atom lst) (list lst))
   ((null (rest lst)) lst)
   (t (apply #'append (mapelt-improper-fixed #'flatten lst)))))

(defun first+n (n lst)
  (if (and n (< n (length lst)))
      (subseq lst 0 n)
      lst))

(defun nth-cons (n lst)
  (cond
    ((and (>= n 0) (< n 1)) lst)
    (lst (nth-cons (- n 1) (rest lst)))
    (t nil)))

(defun nth-cons-from-end (n lst)
  (nth-cons (- (length lst) n) lst))

(defun set-nth! (n lst value)
  (rplaca (nth-cons n lst) value))

(defun indexes (lst)
  (range 0 (length lst)))

(defun set-nth (n lst value)
  (map 'list #'(lambda (i e) (if (eq i n) value e)) (indexes lst) lst))

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
