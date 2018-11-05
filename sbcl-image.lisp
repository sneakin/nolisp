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
      ((or (string= arg "-L")
           (string= arg "--search"))  (values :search-path (second args) (rest (rest args))))
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
  (format *standard-output* "  -L path   Path to search for required files~%")
  (format *standard-output* "~%"))

(defun repl-compiler-toplevel ()
  "Command line entry point for the compiler."
  (multiple-value-bind (options more-args)
      (parse-command-line-args (rest sb-ext:*posix-argv*))
    (let ((input-file (first more-args))
          (output-file (cdr (assoc :output options)))
          (search-path (cdr (assoc :search-path options)))
          (image-root (make-pathname :directory (pathname-directory (pathname (first sb-ext:*posix-argv*))))))
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
                                     (concatenate 'string input-file ".bin")))))))

(defun repl-disasm-disasm (input-file)
  "Disassembles INPUT-FILE to *standard-output*."
  (if (not input-file) (error 'no-input-files-error))
  (format *error-output* "Disassembling ~A~%" input-file)
  (with-open-file (f input-file
                     :direction :input
                     :external-format :default
                     :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence seq f)
      (repl::disassemble-asm seq))))

(defun repl-disasm-help ()
  (format *standard-output* "Options:~%")
  (format *standard-output* "  -help     Print this helpful message~%")
  (format *standard-output* "~%"))

(defun repl-disasm-toplevel ()
  "Command line entry point for the disassembler."
  (multiple-value-bind (options more-args)
      (parse-command-line-args (rest sb-ext:*posix-argv*))
    (let ((input-file (first more-args)))
      (if (assoc :help options)
          (repl-disasm-help)
          (repl-disasm-disasm input-file)))))

(defun repl-save-image (path toplevel)
  "Saves the Lisp core image to PATH using REPL-COMPILER-TOPLEVEL as the init function."
  (save-lisp-and-die path :toplevel toplevel :executable t :purify t))

(defun repl-imager-toplevel (args)
  (let* ((mode (second args))
         (output-path (or (third args)
                          (concatenate 'string mode EXECUTABLE-EXT))))
    (format *error-output* "Saving image for ~A to ~A...~%" mode output-path)
    (case (intern (string-upcase mode) :keyword)
      (:compiler (repl-save-image output-path #'repl-compiler-toplevel))
      (:disassembler (repl-save-image output-path #'repl-disasm-toplevel))
      (t (format *error-output* "Unknown mode: ~A~%" mode)))))

(eval-when (:load-toplevel :execute)
  (handler-case (repl-imager-toplevel sb-ext:*posix-argv*)
    (condition (err) (format *error-output* "Error: ~A~%" err))))
