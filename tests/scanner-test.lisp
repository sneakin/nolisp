(defun test-scan-list-atom-visitor (a state)
  (nassert (atom a) "is an atom")
  (assert-equal state :state)
  'atom)

(defun test-scan-list-list-visitor (l recursor state)
  (nassert (listp l) "is a list")
  (assert-equal state :state)
  'list)

(defun test-scan-list-no-recurse ()
  (let ((scanner (nolisp:partial-after
		  #'scan-list
		  #'test-scan-list-atom-visitor
		  #'test-scan-list-list-visitor
		  :state)))
    ;; calls atom-visitor on atoms
    (assert-equal (funcall scanner '()) 'atom)
    (assert-equal (funcall scanner 123) 'atom)
    ;; calls list-visitor on lists
    (assert-equal (funcall scanner '(a b)) 'list)
    (assert-equal (funcall scanner '(a b (c d) e)) 'list)
    (assert-equal (funcall scanner '(a b (c d (e)))) 'list)))

(defun test-scan-list-recurse-atom-visitor (a state)
  (nassert (atom a) "is an atom")
  'atom)

(defun test-scan-list-recurse-list-visitor (lst recursor state)
  (nassert (listp lst) "is a list")
  ;; builds up state
  (append state (mapcar (partial-after recursor nil) lst)))

(defun test-scan-list-recurse ()
  ;; list-visitor can recurse w/ the same visitors building up state
  (let ((scanner (nolisp:partial-after
		  #'scan-list
		  #'test-scan-list-recurse-atom-visitor
		  #'test-scan-list-recurse-list-visitor
		  )))
    (assert-equal (funcall scanner '()) 'atom)
    (assert-equal (funcall scanner 123) 'atom)
    (assert-equal (funcall scanner '(a b)) '(atom atom))
    (assert-equal (funcall scanner '(a b (c d (e)))) '(atom atom (atom atom (atom))))))

(defun test-scan-list-reducer ()
  (assert-equal (nolisp:scan-list
		 '(1 (2 (3 4)) (((5 6) 7) 8) 9)
		 #'(lambda (item state)
		     (if (oddp item)
			 (list (cons item (first state)) (second state))
		       (list (first state) (cons item (second state)))))
		 #'scan-list-reducer)
		'((9 7 5 3 1) (8 6 4 2))))

(defun test-scan-list ()
  (test-scan-list-no-recurse)
  (test-scan-list-recurse)
  (test-scan-list-reducer))
