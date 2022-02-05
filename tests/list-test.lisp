(defun test-fix-improper-list ()
  (assert-equal (nolisp:fix-improper-list '(1)) '(1))
  (assert-equal (nolisp:fix-improper-list 1) '(1))
  (assert-equal (nolisp:fix-improper-list '(1 . 2)) '(1 2))
  (assert-equal (nolisp:fix-improper-list '(1 2 . 3)) '(1 2 3)))

(defun test-improper-mapcar ()
  (assert-equal (nolisp:improper-mapcar #'identity '(a b c)) '(a b c))
  (assert-equal (nolisp:improper-mapcar #'identity '(a b . c)) '(a b c)))

(defun test-flatten ()
  (assert-equal (nolisp:flatten '(1)) '(1))
  (assert-equal (nolisp:flatten '(1 2)) '(1 2))
  (assert-equal (nolisp:flatten '(1 . 2)) '(1 2))
  (assert-equal (nolisp:flatten '(1 2 . 3)) '(1 2 3))
  (assert-equal (nolisp:flatten '(1 (2 (3)))) '(1 2 3))
  (assert-equal (nolisp:flatten '(1 (2 (3) 4))) '(1 2 3 4))
  (assert-equal (nolisp:flatten '(a nil b)) '(a nil b))
  (assert-equal (nolisp:flatten '(1 (a nil b) 2)) '(1 a nil b 2)))

(defun test-clip-last ()
  (assert-values-equal (nolisp:clip-last '(1)) '(() 1))
  (assert-values-equal (nolisp:clip-last '(1 2)) '((1) 2))
  (assert-values-equal (nolisp:clip-last '(1 2 3)) '((1 2) 3)))

(defun test-nshift-left ()
  (let ((a '(1 2 3)))
    (assert-eq (nolisp:nshift-left a) a "uses the same space"))
  (assert-equal (nolisp:nshift-left '(1)) '(1))
  (assert-equal (nolisp:nshift-left '(1 2)) '(2 1))
  (assert-equal (nolisp:nshift-left '(1 2 3)) '(2 3 1))
  (assert-equal (nolisp:nshift-left '(1 2 3 4)) '(2 3 4 1)))

(defun test-shift-left ()
  (assert-equal (nolisp:shift-left '(1)) '(1))
  (assert-equal (nolisp:shift-left '(1 2)) '(2 1))
  (assert-equal (nolisp:shift-left '(1 2 3)) '(2 3 1))
  (assert-equal (nolisp:shift-left '(1 2 3 4)) '(2 3 4 1)))

(defun test-shift-right ()
  (assert-equal (nolisp:shift-right '(1)) '(1))
  (assert-equal (nolisp:shift-right '(1 2)) '(2 1))
  (assert-equal (nolisp:shift-right '(1 2 3)) '(3 1 2))
  (assert-equal (nolisp:shift-right '(1 2 3 4)) '(4 1 2 3)))

(defun test-reduce-values-fn (i sum diff prod)
  (values (+ i sum) (- diff i) (* i prod)))

(defun test-reduce-values ()
  (assert-equal (nolisp:reduce-values #'+ '(1 2 3 4) :initial-value 0)
		10
		"acts like reduce w/ atomic init")
  (assert-not-equal (nolisp:reduce-values #'- '(1 2 3 4) :initial-value 0)
		    (reduce #'- '(1 2 3 4) :initial-value 0)
		    "but swaps fn argument order")
  (assert-values-equal
   (nolisp:reduce-values #'(lambda (i &optional a b)
			     (values (cons i a) (list i b)))
			 '(1 2 3 4))
   '((4 3 2 1) (4 (3 (2 (1 nil)))))
   "accumulates each value in a separate argument")
  (assert-values-equal
   (nolisp:reduce-values #'test-reduce-values-fn
			 nil
			 :initial-value '(0 0 1))
   '(0 0 1)
   "returns initial values when list is empty")
  (assert-values-equal
   (nolisp:reduce-values #'test-reduce-values-fn
			 '(1 2 3 4)
			 :initial-value '(0 0 1))
   '(10 -10 24)
   "populates initial values from a list")
  (assert-values-equal
   (nolisp:reduce-values #'test-reduce-values-fn
			 '((a 1) (b 2) (c 3) (d 4))
			 :key #'second
			 :from-end t
			 :start 2
			 :end 4
			 :initial-value '(0 0 1))
   '(7 -7 12)
   "supports reduce's keywords"))

(defun test-list ()
  (test-fix-improper-list)
  (test-improper-mapcar)
  (test-flatten)
  (test-clip-last)
  (test-shift-left)
  (test-shift-right)
  (test-reduce-values))
