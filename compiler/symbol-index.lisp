;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/defstruct")

(in-package :repl)

(repl-defstruct symbol-index
                ((max-size)
                 (buffer)
                 (next-offset)))

(defun symbol-index-init (index max-size buffer)
  (set-symbol-index-max-size index max-size)
  (set-symbol-index-buffer index buffer)
  (set-symbol-index-next-offset index buffer))

(defun symbol-index-define (index symbol)
  (set-symbol-index-next-offset index (env-define symbol (symbol-index-buffer index) (symbol-index-next-offset index)))
  index)

(defun symbol-index-position (index)
  (- (symbol-index-next-offset index)
     (symbol-index-buffer index)))

(defun symbol-index-offset (index symbol)
  (env-data-position symbol (symbol-index-buffer index) (symbol-index-next-offset index)))
