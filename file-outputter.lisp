;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "compiler")
(require "outputter")

(in-package :repl)

(defun write-to-file (path output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel)
  (with-open-file (f path
                     :direction :output
                     :if-exists :supersede
                     :external-format :default
                     :element-type '(unsigned-byte 8))
    (let* ((output-end (write-to-array output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel))
           (size (- output-end output)))
      (format *standard-output* "Size ~A~%" size)
      (write-sequence (ptr-read-array output size) f))))

(defun compile-to-file (path output o-offset o-str-end o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
      (repl-compile o-offset o-str-end o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel o-toplevel)
    (write-to-file path output o-code-segment code-segment o-asm-stack asm-stack o-token-offset token-offset env-start env o-toplevel toplevel)))

(defun repl-file (path &optional (buffer-size (length *memory*)) (output-path (concatenate 'string path ".bin")))
  (let ((str-end (ptr-read-file path 0)))
    (compile-to-file output-path
                     (* buffer-size 6)
                     0
                     str-end
                     (* buffer-size 1)
                     (* buffer-size 3)
                     (* buffer-size 5)
                     (* buffer-size 7)
                     (+ (* buffer-size 7) 4)
                     (* buffer-size 8))))
