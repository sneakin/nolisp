#!/usr/bin/env -S ecl --shell

(require :asdf)
(require :nolisp-cli)

(defun build-bin ()
  (asdf:make-build :nolisp-cli
		   :type :program
		   :move-here #P"./"
		   :epilogue-code '(progn
				    (nolisp-cli:main (ext:command-args))
				    (si:exit))))

(eval-when (:execute) (build-bin))
