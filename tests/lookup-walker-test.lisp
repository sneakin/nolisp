(require "assert-match")

(defun test-lookup-walker ()
  (assert-matches '((x x)
                    ((defun f (x y)
                      (* y y (λ (r0) (* x r0 RETURN))))
                     (defun f (x y)
                      (* (argn 1) (argn 1)
                         (λ (:R) (* (lookup 1 0) (argn 0) RETURN))))
                     "lambda forms add a frame")
                    ((lambda (ra fp x) (ra x)) (lambda (ra fp x) (ra (argn 2)))
                     "adds a closing frame pointer")
                    ((if x (return a) (return b))
                     (if x (return a) (return b))
                     "unknown symbols pass through")
                    ((λ (x y) (a x (λ (R0) (b x (λ (R1) (* y R0 R1))))))
                     (λ (x y) (a (argn 0)
                                 (λ (R0) (b (lookup 1 0)
                                            (λ (R1) (* (lookup 2 1)
                                                       (lookup 1 0)
                                                       (argn 0)))))))
                     "nested lambdas as arguments add a frame")
                    ((λ (x) (/ x y
                               (λ (R0)
                                  (to-int R0 (λ (R1)
                                                (if R1
                                                    (return x)
                                                    (return y)))))))
                     (λ (x) (/ (argn 0) y
                               (λ (:R0)
                                  (to-int (argn 0)
                                          (λ (:R1)
                                             (if (argn 0)
                                                 (return (lookup 2 0))
                                                 (return y)))))))
                     "nested lambda forms increase lookup depth"))
                  :fn #'nc-lookup-walker))