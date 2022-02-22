#!/usr/bin/env -S ecl --shell

(eval-when (:execute)
  (require :asdf)
  (require :nolisp-cli))

(in-package :nolisp-cli)

(defun main (args)
  (format t "( Built with NoLisp3 )~%")
  (format *error-output* "( Build Args: )~%  ~s~%  ~s~%" args (ext:command-args))
  (let ((state (reduce #'(lambda (state path)
			   (format t "~%( ~a )~%~%" path)
			   (multiple-value-bind (out new-state)
			       (nolisp:toplevel-compile-file path nolisp:*stages* state :fn #'princ)
			     new-state))
		       (rest args) :initial-value (nolisp:toplevel-default-states))))
    (if (> 2 (length args))
	(nolisp:toplevel-compile-stream *standard-input* nolisp:*stages* state :fn #'princ))))

(defun start ()
  (main ext:*unprocessed-ecl-command-args*))

(eval-when (:execute)
  (start))
