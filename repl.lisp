;;; -*- mode: Lisp; coding: utf-8-unix -*-

#+:sbcl
(defpackage :repl
  (:use :cl)
  (:export :undefined-variable-error
           :unknown-op-error
           :malformed-error
           :malformed-let-error
           :has-feature?
           :*repl-features*
           :env-define
           :env-symbol-position
           :env-stack-position
           :env-push-binding
           :env-pop-bindings
           :read-number
           :read-symbol
           :read-token
           :*TOKEN* :*MEMORY*
           :ptr-read-byte
           :ptr-write-byte
           :ptr-read-array
           :ptr-read-short
           :ptr-write-short
           :ptr-read-long
           :ptr-write-long
           :ptr-read-float
           :ptr-write-float
           :ptr-write-string
           :ptr-read-string
           :ptr-find-string=
           :symbol-string
           :symbol-id
           :space?
           :null?
           :symbol-char?
           :special?
           :alpha?
           :digit?
           :special-form?
           :index-of
           :test
           :test-read-token
           :test-compile
           :test-compile-let
           :test-compile-quote
           :test-compile-set-local
           :test-compile-set-global
           :test-compile-lambda
           :test-compile-named-lambda
           :test-compile-set-lambda
           :test-compile-if
           :test-compile-asm
           :test-compile-values
           :test-compile-mvb
           :test-compile-cond
           :test-compile-conditional
           :test-write-to-array
           :test-write
           :string=
           :repl-read
           :make-op
           :make-short
           :emit-lookup
           :emit-lookup-call
           :emit-value
           :repl-compile
           :repl-eval
           :write-to-array
           :write-to-file
           :compile-to-file
           :repl-file
           ))

#+:sbcl
(in-package :repl)

(require "compiler")
(require "outputter")
(require "file-outputter")
(require "test")
(require "disassembler")

;;;
;;; Compatibility layer functions
;;;
