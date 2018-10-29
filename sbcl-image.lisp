;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Generate an executable image for the compiler.

(load "sbcl.lisp")

#+:WIN32 (defconstant EXECUTABLE-EXT ".exe")
#-:WIN32 (defconstant EXECUTABLE-EXT "")

(defun parse-command-line-arg (args)
  (let ((arg (first args)))
    (cond
      ((or (string= arg "-help")
           (string= arg "--help")
           (string= arg "-h")) (values :help t (rest args)))
      ((or (string= arg "-o")
           (string= arg "--output"))  (values :output (second args) (rest (rest args))))
      (t (values nil nil (rest args) (first args))))))

(defun parse-command-line-args (args &optional options unused)
  (if args
      (multiple-value-bind (name value args unknown)
          (parse-command-line-arg args)
        (parse-command-line-args args
                                 (if name (cons (cons name value) options) options)
                                 (if unknown (cons unknown unused) unused)))
      (values options unused)))

(define-condition no-input-files-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (format stream "No input file specified.~%"))))

(defun repl-compiler-compile (input-file output-file)
  "Compiles INPUT-FILE producing OUTPUT-FILE while giving the user feedback on *error-output*."
  (if (not input-file) (error 'no-input-files-error))
  (format *error-output* "Compiling ~A~%" input-file)
  (repl:repl-file input-file output-file)
  (format *error-output* "Wrote output to ~A~%" output-file))

(defun repl-compiler-help ()
  (format *standard-output* "Options:~%")
  (format *standard-output* "  -help     Print this helpful message~%")
  (format *standard-output* "  -o path   Path of the output file~%")
  (format *standard-output* "~%"))

(defun repl-compiler-toplevel ()
  "Command line entry point for the compiler."
  (multiple-value-bind (options more-args)
      (parse-command-line-args (rest sb-ext:*posix-argv*))
    (let ((input-file (first more-args))
          (output-file (cdr (assoc :output options))))
      (if (assoc :help options)
          (repl-compiler-help)
          (repl-compiler-compile input-file
                                 (if output-file
                                     output-file
                                     (concatenate 'string input-file ".bin")))))))

(defun repl-save-image (path)
  "Saves the Lisp core image to PATH using REPL-COMPILER-TOPLEVEL as the init function."
  (save-lisp-and-die path :toplevel #'repl-compiler-toplevel :executable t :purify t))

(eval-when (:load-toplevel :execute)
  (format *error-output* "Saving image...~%")
  (repl-save-image (concatenate 'string "compiler" EXECUTABLE-EXT)))
