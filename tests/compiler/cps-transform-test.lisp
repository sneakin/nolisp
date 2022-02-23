(defun test-cps-transform ()
  (assert-matches '((abc (RETURN abc))
                    ((boo 1 2 3) (boo 1 2 3 RETURN))
                    ((boo (who))
                     (who (λ (:R) (boo :R RETURN))))
                    ((+ 2 (* 3 4) 5)
                     (* 3 4 (λ (:R) (+ 2 :R 5 RETURN))))
                    ((+ 2 (sqrt (* 3 4)) 5)
                     (* 3 4 (λ (:R0) (sqrt :R0 (λ (:R1) (+ 2 :R1 5 RETURN))))))
		    ((boo #'list) (function list (λ (:R1) (boo :R1 RETURN))) "functions pass through")
		    ((progn)
		     (RETURN nil))
		    ((progn (+ 2 3))
		     (+ 2 3 RETURN))
		    ((progn (+ 2 3) (print 3))
		     (+ 2 3 (λ (:R0) (print 3 (λ (:R1) (progn :R0 :R1 RETURN))))))
		    ((progn (+ 2 3) (print 3) :ok)
		     (+ 2 3 (λ (:R0) (print 3 (λ (:R1) (progn :R0 :R1 :ok RETURN))))))
		    ((* 4 (progn (print 3) (+ 2 3)))
		     (print 3 (λ (:R0) (+ 2 3 (λ (:R1) (progn :R0 :R1 (λ (:R2) (* 4 :R2 RETURN))))))))
                    ((lambda (x) x) (lambda (x) (RETURN x) RETURN))
                    ((lambda (x) (* x x)) (lambda (x) (* x x RETURN) RETURN))
                    ((lambda (x) (print x) (* x x))
		     (lambda (x) (print x (λ (:R0) (* x x (λ (:R1) (progn :R0 :R1 RETURN)))))
		       RETURN))
                    ((if (> a b) (boo a) (boo b))
                     (> a b (λ (TEST)
                               (if TEST
                                   (boo a RETURN)
                                   (boo b RETURN)))))
                    ((if a (boo a) (boo b))
                     (if a
                         (boo a RETURN)
                         (boo b RETURN)))
                    ((if (> a b) a b)
                     (> a b (λ (TEST)
                               (if TEST
                                   (RETURN a)
                                   (RETURN b)))))
                    ((if x (boo)) (if x (boo return) (RETURN nil)))
                    ((who (if (> a b) (boo a) (coo b)))
		     (lambda (?R) (who ?R RETURN)
		       (λ (?CC)
			  (> a b (λ (TEST)
				    (if TEST
					(boo a ?CC)
                                      (coo b ?CC)))))))
                    ((lambda (x y) (if (> x y) x y))
                     (lambda (x y)
                       (> x y (λ (TEST) (if TEST (RETURN x) (RETURN y)))) RETURN))
                    ((boo (lambda (x) (who x)) 3)
                     (lambda (x) (who x RETURN) (λ (:R) (boo :R 3 RETURN))))
                    (((lambda (x) (who x)) 3 4 5)
                     (lambda (x) (who x RETURN) (λ (:R) (:R 3 4 5 RETURN))))
		    ((+ x y z
		      (lambda (x)
			(+ x y z
			   (lambda (y)
			     (+ x y z (lambda (z) (* x y z)))))))
		     (LAMBDA (X)
		       (LAMBDA (Y)
			 (LAMBDA (Z) (* X Y Z RETURN) (λ (?R1) (+ x y z ?R1 RETURN)))
			 (λ (?R2) (+ x y z ?R2 RETURN)))
		       (λ (?R3) (+ x y z ?R3 RETURN)))
		     "lambdas with nested lookups")
                    ((boo (if night you me))
		     (lambda (?R) (boo ?R RETURN)
		       (λ (?CC)
			  (if night (?CC you RETURN) (?CC me RETURN))))) ;; todo pass RETURN?
                    ((defun fn (x) (* x x))
                     (defun fn (x) (* x x RETURN)))
                    ((defun fn (x) (print x) (* x x))
                     (defun fn (x) (print x (λ (:R0) (* x x (λ (:R1) (progn :R0 :R1 RETURN)))))))
		    ((defun squarer () (lambda (x) (* x x)))
		     (defun squarer () (lambda (x) (* x x RETURN) RETURN)))
		    ((defun mapper (lst) (mapcar (lambda (x) (* x x)) lst))
		     (defun mapper (lst)
		       (lambda (x) (* x x RETURN)
			 (λ (:R) (mapcar :R lst RETURN)))))
		    ((boo (if test a b))
		     (lambda (?R) (boo ?R RETURN)
		       (λ (?CC) (if test (?CC a RETURN) (?CC b RETURN))))
		     "simple if statement returning variables as argument")
		    ((boo (if (> a b) a b))
		     (lambda (?R1) (boo ?R1 RETURN)
		       (λ (?CC) (> a b (λ (?R2) (if ?R2 (?CC a RETURN) (?CC b RETURN))))))
		     "simple if statement w/ test returning variables as argument")
		    ((defun f (a) (+ 2 a (if (> a 0) a (- a))))
		     (defun f (a)
		       (lambda (?R) (+ 2 a ?R RETURN)
			 (λ (?CC) (> a 0 (λ (?T) (if ?T (?CC a RETURN) (- a ?CC)))))))
		     "if with variables from a function")
		    ((defun f (a) (+ 2 a (if (> a 0) (* a 2) (* a -2))))
		     (defun f (a)
		       (lambda (?R) (+ 2 a ?R RETURN)
			 (λ (?CC) (> a 0 (λ (?T) (if ?T (* a 2 ?CC) (* a -2 ?CC)))))))
		     "if with variables as argumonts from a function")
		    ((defun f (x y) (if x (if y t)))
		     (defun f (x y)
		       (if x
			   (if y (RETURN t) (RETURN nil))
			   (RETURN nil)))
		     "defun with nested IFs"))
		  :fn #'nolisp:cps-transform))
