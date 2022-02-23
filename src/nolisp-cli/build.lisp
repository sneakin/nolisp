(require :asdf)
(require :nolisp-cli)

(defun build-bin ()
  (asdf:operate 'asdf:program-op :nolisp-cli)
  (format t "Wrote ~a~%"
          (first (asdf:output-files 'asdf:program-op :nolisp-cli))))

(eval-when (:execute) (build-bin))
