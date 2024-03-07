;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime")
(require "compiler")
(require "runtime/bc/io/console")
(require "runtime/bc/io/input-dev")
(require "runtime/bc/io/output-dev")
(require "logging")

(defun package-eval (package str)
  (with-allocation (asm-stack 4096)
    (with-allocation (env 4096)
      (console-write ";; Evaluating:")
      (console-write str)
      (multiple-value-bind (req-offset asm-stack env kind value)
          (repl-compile package str (+ str (length str)) asm-stack env env)
        (console-write ";; => ")
        (console-write-unsigned-integer req-offset 16)
        (console-write-unsigned-integer asm-stack 16)
        (console-write-unsigned-integer env 16)
        (console-write-unsigned-integer kind 16)
        (console-write-unsigned-integer value 16))
      )))

(defvar segment-size 4096)

(defun package-info (package)
  (console-write "Package addresses")
  (console-write-unsigned-integer (package-code-segment-buffer package) 16)
  (console-write-unsigned-integer (package-code-segment-offset package) 16)
  (console-write-unsigned-integer (package-string-segment-data package) 16)
  (console-write-unsigned-integer (package-string-segment-offset package) 16)
  (console-write-unsigned-integer (package-symbols-buffer package) 16)
  (console-write-unsigned-integer (package-symbols-next-offset package) 16))

(defun with-package (fn &optional args)
  (console-write "with-package")
  (with-allocation (package 64) ; (package-size)
    (with-allocation (CS 4096)
      (with-allocation (DS 4096)
        (with-allocation (toplevel 4096)
          (with-allocation (libs 4096)
            (with-allocation (imports 4096)
              (with-allocation (exports 4096)
                (with-allocation (source-files 4096)
                  (console-write "package-init")
                  (package-init package
                                CS segment-size
                                DS segment-size
                                toplevel segment-size
                                source-files segment-size
                                libs segment-size
                                imports segment-size
                                exports segment-size)
                  (package-info package)
                  (fn package args))))))))))

(defun repl-eval (str)
  (console-write "repl-eval")
  (console-write str)
  (with-package (lambda (pkg data)
                  (console-write "repl-eval lambda")
                  (console-write data)
                  (package-eval pkg data))
    str))

(defun repl-loop (pkg data)
  (output-dev-write "> ")
  (with-allocation (line 1024)
    (if (input-dev-readline line)
        (progn
          (output-dev-write "; <= ")
          (output-dev-write line)
          (output-dev-write ";=> ")
          (output-dev-write-integer (package-eval pkg line))
          (output-dev-write "\n")
          (repl-loop pkg data)))))

                                        ;(with-package repl-loop/2)

(console-write-integer (eq :hello :HELLO))
(console-write-integer (eq 'hello :HELLO))
(console-write "Hello string")
(console-write :hello)
(console-write-unsigned-integer :hello 16)
(console-write-unsigned-integer (symbol-name :hello) 16)
(console-write (symbol-name :hello))
(console-write (symbol-name 'HELLO))
(console-write-integer (repl-eval "(eq :hello :HELLO)"))

(logger-init LOGGER-LEVEL-DEBUG)

(repl-eval
 "(def + (a b)
  (asm (load 0 0 11) 8
       (load 1 0 11) 4
       (cls 7)
       (addi 1 14)))
(+ 4 5)
(values (+ 1 1) (+ 2 2) (+ 3 3))")
                                        ;(repl-eval "(values 1 2 3 4)")
;(repl-eval "12345")
(console-write "Bye.")
;(read-token)
(halt)
