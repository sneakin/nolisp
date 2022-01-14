(defun test-scan-list-atom-visitor (a state)
  (nassert (atom a) "is an atom")
  (assert-equal state :state)
  'atom)

(defun test-scan-list-list-visitor (l recursor state)
  (nassert (listp l) "is a list")
  (assert-equal state :state)
  'list)

(defun test-scan-list-no-recurse ()
  ;; calls atom-visitor on atoms
  ;; calls list-visitor on lists
  ;; passes along state
  ;; list-visitor can recurse w/ the same visitors and state
  (let ((scanner (nolisp:partial-after
		  #'scan-list
		  #'test-scan-list-atom-visitor
		  #'test-scan-list-list-visitor
		  :state
		  )))
    (assert-equal (funcall scanner '()) 'atom)
    (assert-equal (funcall scanner 123) 'atom)
    (assert-equal (funcall scanner '(a b)) 'list)
    (assert-equal (funcall scanner '(a b (c d) e)) 'list)
    (assert-equal (funcall scanner '(a b (c d (e)))) 'list)))

(defun test-scan-list-recurse-atom-visitor (a state)
  (nassert (atom a) "is an atom")
  'atom)

(defun test-scan-list-recurse-list-visitor (lst recursor state)
  (nassert (listp l) "is a list")
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

(defun test-scan-list ()
  (test-scan-list-no-recurse)
  (test-scan-list-recurse))
