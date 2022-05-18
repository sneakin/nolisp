(eval-when (:execute)
  (require :asdf)
  (require :nolisp-cli))

(in-package :nolisp-cli)

(defun command-args ()
  #+ecl (or ext:*unprocessed-ecl-command-args* (ext:command-args))
  #+sbcl sb-ext:*posix-argv*)
 
(defun select-stages (name stages)
  (if name
      (nolisp::copy-assoc-until-n (intern (string-upcase name) 'nolisp) stages)
      stages))

(defun print-usage (cmd)
  (format t "Usage: ~a [-s stage] [-skip stage]* [-debug] [-help] files...~%~%" cmd)
  (format t "Stages: ~A~%" (mapcar #'first nolisp:*stages*)))

(defun compile-paths (paths stages state)
  (reduce
   #'(lambda (state path)
       (format t "~%( ~a )~%~%" path)
       (multiple-value-bind (out new-state)
	   (nolisp:toplevel-compile-file path stages state :fn #'princ)
	 new-state))
   paths :initial-value state))

(defun main (args)
  (multiple-value-bind (options paths)
      (parse-options (rest args) :collections '("skip"))
    (let* ((debugging (option-value "debug" options))
	   (skipping (mapcar #'(lambda (i) (intern (string-upcase i) :nolisp))
			     (nolisp:assoc-get-all "skip" options)))
	   (stage (option-value "s" options))
	   (stages (remove-if #'(lambda (i) (position (first i) skipping))
			      (select-stages stage nolisp:*stages*)))
	   (states (nolisp:toplevel-default-states stages)))
      (when debugging
	(format *error-output* "( Build Args: )~%  ~s~%  ~s~%" args (command-args))
	(format *error-output* "( Options: ~s )~%" options)
	(format *error-output* "( Paths: ~s )~%" paths)
	(format *error-output* "( Stages: ~a )~%" (mapcar #'first stages))
	(format *error-output* "( Skipped: ~s )~%" skipping))
      (cond
	((option-value "help" options) (print-usage (first args)))
	((eq nil paths)
	 (format t "( NoLisp3 )~%")
	 (multiple-value-bind (output states)
	     (nolisp:toplevel-compile-stream *standard-input* stages states :fn #'princ)
	   (when debugging (print states))))
	(t (format t "( Built with NoLisp3 )~%")
	   (multiple-value-bind (states)
	       (compile-paths paths stages states)
	     (when debugging (print states))))))))

(defun start ()
  (main (command-args)))
