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

(defun compile-to-file (path state output o-offset o-str-end o-asm-stack env-start o-env)
  (multiple-value-bind (offset asm-stack env)
      (repl-compile state o-offset o-str-end o-asm-stack env-start o-env)
    (write-to-file path
                   output
                   (package-code-segment-buffer state)
                   (package-code-segment-offset state)
                   o-asm-stack
                   asm-stack
                   (package-string-segment-data state)
                   (package-string-segment-offset state)
                   env-start
                   env
                   (package-symbols-buffer state)
                   (package-symbols-next-offset state))))

(defun repl-file (path &optional (buffer-size (ceiling (/ (length *memory*) 9))) (output-path (concatenate 'string path ".bin")))
  (let ((str-end (ptr-read-file path 0)))
    (with-allocation (state (package-size))
      (package-init state buffer-size (* 8 buffer-size))
      (compile-to-file output-path
                       state
                       (* buffer-size 6)
                       0
                       str-end
                       (* buffer-size 3)
                       (* buffer-size 7)
                       (+ (* buffer-size 7) 4)))))

