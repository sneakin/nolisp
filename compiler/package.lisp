;;; -*- mode: Lisp; coding: utf-8-unix -*-

#-:repl (require "runtime/defstruct")
#+:repl (require "runtime/package")

(require "compiler/byte-buffer")
(require "compiler/symbol-index")
(require "compiler/imports-list")

#-:repl (in-package :repl)

(defvar *INIT-FUNC* "__init")

#+:repl (require "bootstrap/package")
#-:repl
(repl-defstruct package
  ((code-segment :type 'byte-buffer)
   (string-segment :type 'byte-buffer)
   (symbols :type 'symbol-index)
   (source-files :type 'byte-buffer)
   (libs :type 'symbol-index)
   (imports :type 'imports-list)
   (exports :type 'symbol-index)))

(defun package-define (compiler symbol)
  (symbol-index-define (package-symbols compiler) symbol)
  compiler)

(defun package-symbols-buffer (compiler)
  (symbol-index-buffer (package-symbols compiler)))

(defun package-symbol-offset (compiler symbol)
  (symbol-index-offset (package-symbols compiler) symbol))

(defun package-symbols-next-offset (compiler)
  (symbol-index-next-offset (package-symbols compiler)))

(defun package-symbols-size (compiler)
  (symbol-index-position (package-symbols compiler)))

(defun package-get-lib (compiler n)
  (symbol-index-get (package-libs compiler) n))

(defun package-get-lib-name (compiler n)
  (let ((lib (package-get-lib compiler n)))
    (if lib
        (ptr-read-string (ptr-read-ptr lib)))))

(defun package-require-lib (compiler lib)
  (symbol-index-define (package-libs compiler) lib)
  (- (symbol-index-count (package-libs compiler))
     1))

(defun package-libs-size (compiler)
  (symbol-index-position (package-libs compiler)))

(defun package-libs-count (compiler)
  (symbol-index-count (package-libs compiler)))

(defun package-get-import (compiler n)
  (imports-list-get (package-imports compiler) n))

(defun package-get-import-fun-name (compiler n)
  (let ((import (package-get-import compiler n)))
    (if import
        (ptr-read-string (import-entry-fun-name import)))))

(defun package-get-import-lib-index (compiler n)
  (let ((import (package-get-import compiler n)))
    (if import
        (import-entry-lib import)
        -1)))

(defun package-get-import-lib (compiler n)
  (let ((lib (package-get-import-lib-index compiler n)))
    (if (>= lib 0)
        (package-get-lib compiler lib))))

(defun package-get-import-lib-name (compiler n)
  (let ((lib (package-get-import-lib-index compiler n)))
    (if (>= lib 0)
        (package-get-lib-name compiler lib))))

(defun package-import (compiler lib-index symbol)
  (imports-list-define (package-imports compiler) lib-index symbol)
  compiler)

(defun package-imports-size (compiler)
  (imports-list-position (package-imports compiler)))

(defun package-imports-count (compiler)
  (imports-list-count (package-imports compiler)))

(defun package-count-imports-from-lib (compiler lib-index &optional (import-index 0) (count 0))
  (if (< import-index (package-imports-count compiler))
      (package-count-imports-from-lib compiler lib-index (+ import-index 1) (if (eq lib-index (package-get-import-lib-index compiler import-index))
                                                                                (+ count 1)
                                                                                count))
      count))

(defun package-get-export (compiler n)
  (symbol-index-get (package-exports compiler) n))

(defun package-get-export-name (compiler n)
  (let ((export (package-get-export compiler n)))
    (if export
        (ptr-read-string (ptr-read-ptr export)))))

(defun package-export (compiler symbol)
  (symbol-index-define (package-exports compiler) symbol)
  compiler)

(defun package-exports-size (compiler)
  (symbol-index-position (package-exports compiler)))

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

(defun package-string-segment-position (compiler)
  (byte-buffer-position (package-string-segment compiler)))

(defun package-copy-to-string-segment (compiler src num-bytes)
  (byte-buffer-copy (package-string-segment compiler) src num-bytes))

(require "symbol-gen")

(defun package-symbol-gen (state)
  (multiple-value-bind (name offset)
      (symbol-gen (package-string-segment-offset state) (package-string-segment-data state))
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

(defun package-init (compiler
                     cs cs-size
                     ds ds-size
                     toplevel top-size
                     src-files sf-size
                     libs libs-size
                     imports imports-size
                     exports exports-size)
  (byte-buffer-init (package-code-segment compiler)
                    cs
                    cs-size)
  (byte-buffer-init (package-string-segment compiler)
                    ds
                    ds-size)
  (symbol-index-init (package-symbols compiler)
                     top-size
                     toplevel)
  (symbol-index-init (package-libs compiler)
                     libs-size
                     libs)
  (imports-list-init (package-imports compiler)
                     imports-size
                     imports)
  (symbol-index-init (package-exports compiler)
                     exports-size
                     exports)
  (let ((init-sym (package-intern compiler *INIT-FUNC*)))
    (package-define compiler init-sym)
    (package-export compiler init-sym))
  (package-define compiler (package-intern compiler "*STRING-SEGMENT*"))
  (byte-buffer-init (package-source-files compiler)
                    src-files
                    sf-size)
  compiler)
