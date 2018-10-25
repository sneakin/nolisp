;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl (require "runtime/defstruct")
#+:repl (require "runtime/package")

(require "compiler/byte-buffer")
(require "compiler/symbol-index")

(in-package :repl)

(defvar *INIT-FUNC* "__init")

#+:repl (require "bootstrap/package")
#-:repl
(repl-defstruct package
  ((code-segment :type 'byte-buffer)
   (string-segment :type 'byte-buffer)
   (symbols :type 'symbol-index)
   (source-files :type 'byte-buffer)))

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

(require "symbol-gen")

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

(defun package-add-source-file (package src-path)
  (byte-buffer-copy-string (package-source-files package) src-path)
  package)

(defun package-required? (package src-path)
  (ptr-find-string-equal src-path (byte-buffer-data (package-source-files package)) (byte-buffer-offset (package-source-files package))))

(defun package-init (compiler cs cs-size ds ds-size toplevel top-size src-files sf-size)
  (byte-buffer-init (package-code-segment compiler)
                      cs
                      cs-size)
    (byte-buffer-init (package-string-segment compiler)
                      ds
                      ds-size)
    (symbol-index-init (package-symbols compiler)
                       top-size
                       toplevel)
    (package-define compiler (package-intern compiler *INIT-FUNC*))
    (package-define compiler (package-intern compiler "*STRING-SEGMENT*"))
    (byte-buffer-init (package-source-files compiler)
                      src-files
                      sf-size)
    compiler)
