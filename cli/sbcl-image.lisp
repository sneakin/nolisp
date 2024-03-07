;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Generate an executable image for the compiler.

;;(eval-when (:execute)
;;  (load "loader.lisp"))

(require :repl-loader "loader.lisp")
(require "logging")
(require "cli/arguments")

#+:WIN32 (defconstant EXECUTABLE-EXT ".exe")
#-:WIN32 (defconstant EXECUTABLE-EXT "")

(defun repl-save-image (path toplevel)
  "Saves the Lisp core image to PATH using REPL-COMPILER-TOPLEVEL as the init function."
  (save-lisp-and-die path :toplevel toplevel :executable t :purify t))

(defun repl-imager-toplevel (args)
  (let* ((mode (second args))
         (output-path (or (third args)
                          (concatenate 'string mode EXECUTABLE-EXT))))
    (format *error-output* "Saving image for ~A to ~A...~%" mode output-path)
    (case (intern (string-upcase mode) :keyword)
      (:compiler
        (require "cli/compiler")
        (repl-save-image output-path 'repl-compiler-toplevel))
      (:disassembler
        (require "cli/disasm")
        (repl-save-image output-path 'repl-disasm-toplevel))
      (t (format *error-output* "Unknown mode: ~A~%" mode)))))

(eval-when (:execute)
  (handler-case (repl-imager-toplevel (remove-shell-args (command-args)))
    (condition (err) (format *error-output* "Error: ~A~%~A~%" (type-of err) err))))
