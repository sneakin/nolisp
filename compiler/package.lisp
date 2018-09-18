;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/defstruct")
(require "compiler/byte-buffer")
(require "compiler/symbol-index")
(require "symbol-gen")

(in-package :repl)

(repl-defstruct package
  ((code-segment :type 'byte-buffer)
   (string-segment :type 'byte-buffer)
   (symbols :type 'symbol-index)))

(defun package-init (compiler buffer buffer-size)
  (let ((segment-size (ceiling (/ buffer-size 9))))
    (byte-buffer-init (package-code-segment compiler) (+ buffer (* segment-size 1)) segment-size)
    (byte-buffer-init (package-string-segment compiler) (+ buffer (* segment-size 5)) segment-size)
    (symbol-index-init (package-symbols compiler) segment-size (+ buffer (* segment-size 8)))))

(defun package-define (compiler symbol)
  (symbol-index-define (package-symbols compiler) symbol)
  compiler)

(defun package-symbols-buffer (compiler)
  (symbol-index-buffer (package-symbols compiler)))

(defun package-symbol-offset (compiler symbol)
  (symbol-index-offset (package-symbols compiler) symbol))

(defun package-symbols-next-offset (compiler)
  (symbol-index-next-offset (package-symbols compiler)))

(defun package-code-segment-buffer (compiler)
  (byte-buffer-data (package-code-segment compiler)))

(defun package-code-segment-position (compiler)
  (byte-buffer-position (package-code-segment compiler)))

(defun package-code-segment-offset (compiler)
  (byte-buffer-offset (package-code-segment compiler)))

(defun package-copy-to-code-segment (compiler src num-bytes)
  (byte-buffer-copy (package-code-segment compiler) src num-bytes)
  compiler)

(defun set-package-string-segment-data (compiler ptr)
  (set-byte-buffer-data (package-string-segment compiler) ptr)
  (set-byte-buffer-position (package-string-segment compiler) 0)
  compiler)

(defun package-string-segment-data (compiler)
  (byte-buffer-data (package-string-segment compiler)))

(defun set-package-string-segment-position (compiler n)
  (set-byte-buffer-position (package-string-segment compiler) n)
  compiler)

(defun set-package-string-segment-offset (compiler offset)
  (set-byte-buffer-offset (package-string-segment compiler) offset)
  compiler)

(defun package-string-segment-offset (compiler)
  (byte-buffer-offset (package-string-segment compiler)))

(defun package-string-segment-end (compiler)
  (+ (byte-buffer-data (package-string-segment compiler))
     (byte-buffer-position (package-string-segment compiler))))

(defun package-copy-to-string-segment (compiler src num-bytes)
  (byte-buffer-copy (package-string-segment compiler) src num-bytes)
  compiler)

(defun package-symbol-gen (state)
  (multiple-value-bind (name offset)
      (symbol-gen (package-string-segment-offset state))
    (set-package-string-segment-offset state offset)
    name))

(defun package-intern (state str)
  (multiple-value-bind (sym offset)
      (symbol-intern str (package-string-segment-data state) (package-string-segment-offset state))
    (set-package-string-segment-offset state offset)
    sym))
