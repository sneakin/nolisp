(require "testing/nassert")

(defun setup-test ()
  (nc-add-macro 'boo #'(lambda (x y) `(+ ,x ,y)))
  (nc-add-macro 'who #'(lambda (x y) `(boo ,x ,y))))

(defun teardown-test ()
  (nc-remove-macro 'boo)
  (nc-remove-macro 'who)
  nil)

(defun test-nc-macroexpand-1 ()
  (setup-test)
  (assert-equal (nc-macroexpand-1 '(who (boo 3 4) (who (boo 4 5) 7)))
                '(boo (+ 3 4) (boo (+ 4 5) 7)))
  (teardown-test))

(defun test-nc-macroexpand ()
  (setup-test)
  (assert-equal (nc-macroexpand '(who (boo 3 4) (who (boo 4 5) 7)))
                '(+ (+ 3 4) (+ (+ 4 5) 7)))
  (teardown-test))
