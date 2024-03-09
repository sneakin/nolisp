#-:ecl
(eval-when (:execute :load-toplevel)
  (require :repl-loader "loader.lisp")
  (require "logging")
  (require "cli/arguments"))

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

(defun repl-disasm-toplevel (&optional (args (command-args)))
  "Command line entry point for the disassembler."
  (multiple-value-bind (options more-args)
      (parse-command-line-args (rest args))
    (let ((input-file (first more-args)))
      (if (assoc :help options)
          (repl-disasm-help)
          (repl-disasm-disasm input-file)))))
