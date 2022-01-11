#+sbcl
(defun load-test-units ()
  (let ((testdir (namestring (first (directory "./")))))
    (mapcar #'(lambda (p)
                (let ((name (subseq (namestring p) (length testdir))))
                  (require (subseq name 0 (- (length name) 5)))) ; remove the ".lisp"
                )
            (directory "./tests/**/*-test.lisp"))))

#-sbcl
(defun load-test-units ()
  (let ((testdir (namestring (first (directory "./")))))
    (mapcar #'(lambda (p) (print p) (load p))
            (directory "./tests/**/*-test.lisp"))))

(load-test-units)

