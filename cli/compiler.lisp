(require :repl-loader "loader.lisp")
(require "logging")
(require "cli/arguments")

(define-condition no-input-files-error (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
             (format stream "No input file specified.~%"))))

(defun repl-compiler-compile (input-file output-file &optional (data-segment-offset (* 2 1024 1024)))
  "Compiles INPUT-FILE producing OUTPUT-FILE while giving the user feedback on *error-output*."
  (if (not input-file) (error 'no-input-files-error))
  (format *error-output* "Compiling ~A~%" input-file)
  (repl:repl-file input-file data-segment-offset output-file)
  (format *error-output* "Wrote output to ~A~%" output-file))

(defun repl-compiler-help ()
  (format *standard-output* "Options:~%")
  (format *standard-output* "              -help  Print this helpful message~%")
  (format *standard-output* "            -o path  Path of the output file~%")
  (format *standard-output* "            -L path  Path to search for required files~%")
  (format *standard-output* "                -ds  address  Address to locate the data segment~%")
  (format *standard-output* "  --log-level level  Maximum level of log messages to print.~%")
  (format *standard-output* "          --no-tail  Do not optimize tail calls.~%")
  (format *standard-output* "~%"))

(defun repl-compiler-toplevel (&optional (args (command-args)))
  "Command line entry point for the compiler."
  (multiple-value-bind (options more-args)
      (parse-command-line-args (rest args))
    (let ((input-file (first more-args))
          (output-file (cdr (assoc :output options)))
          (search-path (cdr (assoc :search-path options)))
          (data-segment-offset (or (cdr (assoc :data-segment options)) (* 2 1024 1024)))
          (image-root (make-pathname :directory (pathname-directory (pathname (first args)))))
          (log-level (or (cdr (assoc :log-level options)) :info))
          (no-tail (cdr (assoc :no-tail options))))
      ;; set logger level and reset logger stream
      (repl::logger-init log-level)
      (setq repl::*allow-tail-calls* (not no-tail))
      ;; add the binary's path to the load-path
      (push image-root repl::*load-path*)
      (push (merge-pathnames (make-pathname :name "runtime") image-root) repl::*load-path*)
      ;; and add the path from the command line
      (if search-path (push (make-pathname :directory `(:relative ,search-path)) repl::*load-path*))
      ;; command mode dispatch:
      (if (assoc :help options)
          (repl-compiler-help)
          (repl-compiler-compile input-file
                                 (if output-file
                                     output-file
                                     (concatenate 'string input-file ".bin"))
                                 data-segment-offset)))))
