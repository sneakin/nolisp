;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "compiler")
(require "outputter")

(in-package :repl)

(defun write-to-file (path output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel data-segment-offset)
  (with-open-file (f path
                     :direction :output
                     :if-exists :supersede
                     :external-format :default
                     :element-type '(unsigned-byte 8))
    (let* ((output-end (write-to-array output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel data-segment-offset))
           (size (- output-end output)))
      (format *standard-output* "Size ~A~%" size)
      (write-sequence (ptr-read-array output size) f))))

(defun compile-to-file (path state output o-offset o-str-end o-asm-stack env-start o-env data-segment-offset)
  (format *standard-output* "CS start: ~A~%" (package-code-segment-buffer state))
  (multiple-value-bind (offset asm-stack env)
      (repl-compile state o-offset o-str-end o-asm-stack env-start o-env)
    (format *standard-output* "CS finish: ~A~%" (package-code-segment-buffer state))
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
                   (package-symbols-next-offset state)
                   data-segment-offset)))

(defun repl-file (path &optional (data-segment-offset 0) (output-path (concatenate 'string path ".bin")) (buffer-size (ceiling (/ (length *memory*) 8))) (offset 0))
  (let ((str-end (ptr-read-file path offset)))
    (with-allocation (state (package-size))
      (package-init state
                    (+ offset (* buffer-size 1))
                    buffer-size
                    (+ offset (* buffer-size 2))
                    buffer-size
                    (+ offset (* buffer-size 3))
                    buffer-size
                    (+ offset (* buffer-size 4))
                    buffer-size)
      (compile-to-file output-path
                       state
                       (+ offset (* buffer-size 5))
                       0
                       str-end
                       (+ offset (* buffer-size 6))
                       (+ offset (* buffer-size 7))
                       (+ (+ offset (* buffer-size 7)) 4)
                       data-segment-offset))))

