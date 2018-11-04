;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime")
(require "compiler")

(defun package-eval (str package)
  (with-allocation (asm-stack 4096)
    (with-allocation (env 4096)
      (repl-compile package str (+ str (length str)) asm-stack env env))))

(defun repl-eval (str &optional (segment-size 4096))
  (with-allocation (package 64) ; (package-size)
    (with-allocation (CS 4096)
      (with-allocation (DS 4096)
        (with-allocation (toplevel 4096)
          (with-allocation (source-files 4096)
            (package-init package
                          CS segment-size
                          DS segment-size
                          toplevel segment-size
                          source-files segment-size)
            (package-eval package str)))))
    ))

(repl-eval "(values (+ 1 2 3 4) (+ (* 2 2) (* 3 3)")
;(read-token)
(halt)
