;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/defstruct")

(in-package :repl)

(repl-defstruct byte-array
                ((length :initform 0)
                 (data)))

(repl-defstruct byte-buffer
                ((data)
                 (length :initform 0)
                 (position :initform 0)))

(defun byte-buffer-write (buffer c)
  (ptr-write-byte c (+ (byte-buffer-data buffer)
                       (byte-buffer-position buffer)))
  (set-byte-buffer-position buffer (+ (byte-buffer-position buffer) 1))
  buffer)

(defun byte-buffer-read (buffer)
  (let ((b (ptr-read-byte (+ (byte-buffer-data buffer)
                             (byte-buffer-position buffer)))))
    (set-byte-buffer-position buffer (+ (byte-buffer-position buffer) 1))
    b))


(repl-defstruct symbol-index
                ((symbols :size 1)))

(repl-defstruct compiler
  ((input :type 'byte-buffer)
   (code-segment :type 'byte-buffer)
   (asm-stack :type 'byte-buffer)
   (token-segment :type 'byte-buffer)
   (env :type 'symbol-index)
   (toplevel :type 'symbol-index)))

(defun set-compiler-token-segment-data (compiler ptr)
  (set-byte-buffer-data (compiler-token-segment compiler) ptr)
  (set-byte-buffer-position (compiler-token-segment compiler) 0)
  compiler)

(defun compiler-token-segment-data (compiler)
  (byte-buffer-data (compiler-token-segment compiler)))

(defun set-compiler-token-segment-position (compiler)
  (set-byte-buffer-position (compiler-token-segment compiler))
  compiler)

(defun compiler-token-segment-end (compiler)
  (+ (byte-buffer-data (compiler-token-segment compiler))
     (byte-buffer-position (compiler-token-segment compiler))))

(defun compiler-update-input (compiler new-position))
(defun compiler-copy-to-code-segment (compiler ptr bytes))
(defun compiler-copy-to-asm-stack (compiler ptr bytes))
(defun compiler-copy-to-token-segment (compiler ptr bytes))

(defun compiler-global-init ()
  (setq *COMPILER* (allocate (compiler-size)))
  (compiler-init *COMPILER*))
