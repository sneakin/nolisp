(defun setup-test ()
  (nolisp:add-macro 'boo #'(lambda (x y) `(+ ,x ,y)))
  (nolisp:add-macro 'who #'(lambda (x y) `(boo ,x ,y)))
  (nolisp:add-macro 'woo #'(lambda (x) `(who ,x ,x))))

(defun teardown-test ()
  (nolisp:remove-macro 'boo)
  (nolisp:remove-macro 'who)
  (nolisp:remove-macro 'woo)
  nil)

(defun test-macro-expand-1 ()
  (setup-test)
  (assert-equal (nolisp:macro-expand-1 '(who (boo 3 4) (who (boo 4 5) 7)))
                '(boo (boo 3 4) (who (boo 4 5) 7)))
  (assert-equal (nolisp:macro-expand-1 '(boo (boo 3 4) (who (boo 4 5) 7)))
                '(+ (boo 3 4) (who (boo 4 5) 7)))
  (assert-equal (nolisp:macro-expand-1 '(+ (boo 3 4) (who (boo 4 5) 7)))
                '(+ (+ 3 4) (boo (boo 4 5) 7)))
  (assert-equal (nolisp:macro-expand-1 '(+ (+ 3 4) (boo (boo 4 5) 7)))
                '(+ (+ 3 4) (+ (boo 4 5) 7)))
  (assert-equal (nolisp:macro-expand-1 '(+ (+ 3 4) (+ (boo 4 5) 7)))
                '(+ (+ 3 4) (+ (+ 4 5) 7)))
  (assert-equal (nolisp:macro-expand-1 '(woo (woo 3))) '(who (woo 3) (woo 3)))
  (assert-equal (nolisp:macro-expand-1 '(who (woo 3) (woo 3))) '(boo (woo 3) (woo 3)))
  (assert-equal (nolisp:macro-expand-1 '(boo (woo 3) (woo 3))) '(+ (woo 3) (woo 3)))
  (assert-equal (nolisp:macro-expand-1 '(+ (woo 3) (woo 3))) '(+ (who 3 3) (who 3 3)))
  (assert-equal (nolisp:macro-expand-1 '(+ (who 3 3) (who 3 3))) '(+ (boo 3 3) (boo 3 3)))
  (assert-equal (nolisp:macro-expand-1 '(+ (boo 3 3) (boo 3 3))) '(+ (+ 3 3) (+ 3 3)))
  (teardown-test))

(defun test-macro-expand ()
  (setup-test)
  (assert-equal (nolisp:macro-expand '(who (boo 3 4) (who (boo 4 5) 7)))
                '(+ (+ 3 4) (+ (+ 4 5) 7)))
  (assert-equal (nolisp:macro-expand '(woo 3)) '(+ 3 3))
  (assert-equal (nolisp:macro-expand '(woo (woo 3))) '(+ (+ 3 3) (+ 3 3)))
  (assert-equal (nolisp:macro-expand '(let ((x (+ 2 3))
					    (y (* 4 2)))
					(+ x y)))
		'((lambda (x y) (+ x y)) (+ 2 3) (* 4 2)))
  ;; needs to handle earlier references
  ;; (assert-equal (nolisp:macro-expand '(let ((x (+ 2 3))
  ;; 					    (y (* x 2)))
  ;; 					(+ x y)))
  ;; 		'(funcall (lambda (x y) (+ x y)) (+ 2 3) (* x 2)))
  (teardown-test))
