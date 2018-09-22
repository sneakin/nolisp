;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime")
(require "compiler")

#+:bah
(defun package-eval (str package)
  (with-allocation (asm-stack stack-size)
    (with-allocation (env (stack-size))
      (repl-compile package str (+ str (length str)) asm-stack env env))))

#+:bah
(defun repl-eval (str &optional (buffer-size 4096))
  (with-allocation (package (package-size))
    (with-allocation (CS buffer-size)
      (with-allocation (DS buffer-size)
        (with-allocation (toplevel buffer-size)
          (with-allocation (source-files buffer-size)
            (package-init package
                          CS segment-size
                          DS segment-size
                          toplevel segment-size
                          src-files segment-size)
            (package-eval package str)))))
    ))

#+:bah (repl-eval "(values (+ 1 2 3 4) (+ (* 2 2) (* 3 3)")
#+:bah (asm (halt))
