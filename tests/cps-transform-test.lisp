(require "assert-match")

(defun test-cps-transform ()
  (assert-matches '((abc (return abc))
                    ((boo 1 2 3) (boo 1 2 3 RETURN))
                    ((boo (who))
                     (who (λ (:R) (boo :R RETURN))))
                    ((lambda (x) x) (lambda (:return :fp x) (:return x)))
                    ((lambda (x) (* x x)) (lambda (:return :fp x) (* x x :return)))
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
                    ((if x (boo)) (if x (boo return) (return nil)))
                    ((who (if (> a b) (boo a) b))
                     (> a b (λ (TEST)
                               (if TEST
                                   (boo a (λ (:R) (who :R RETURN)))
                                   (who b RETURN)))))
                    ((lambda (x y) (if (> x y) x y))
                     (lambda (:return :fp x y)
                       (> x y (λ (TEST) (if TEST (:return x) (:return y))))))
                    ((boo (lambda (x) (who x)) 3)
                     (boo (lambda (:return :fp x) (who x :return)) 3 RETURN))
                    ((boo (if night you me))
                     (if night (boo you RETURN) (boo me RETURN)))
                    ((defun fn (x) (* x x))
                     (defun fn (:return x) (* x x :return))))
                  :fn #'nc-cps-transform))