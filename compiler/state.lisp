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

(defun byte-buffer-end (buffer)
  (+ (byte-buffer-data buffer)
     (byte-buffer-length buffer)))

(defun byte-buffer-offset (buffer)
  (+ (byte-buffer-data buffer)
     (byte-buffer-position buffer)))

(defun set-byte-buffer-offset (buffer offset)
  (set-byte-buffer-position buffer (- offset (byte-buffer-data buffer))))

#+:sbcl
(define-condition byte-buffer-overflow-error (repl-error)
  ((buffer :initarg :buffer :initform nil)
   (index :initarg :index :initform nil))
  (:report (lambda (condition stream)
             (format stream "Index ~A overflows buffer(~A)~%"
                     (slot-value condition 'index)
                     (byte-buffer-length (slot-value condition 'buffer))))))

(defun byte-buffer-write (buffer c)
  (if (>= (byte-buffer-offset buffer) (byte-buffer-end buffer))
      (error 'byte-buffer-overflow-error :buffer buffer :index (byte-buffer-position buffer)))
  (ptr-write-byte c (byte-buffer-offset buffer))
  (set-byte-buffer-position buffer (+ (byte-buffer-position buffer) 1))
  buffer)

(defun byte-buffer-copy (buffer src num-bytes)
  (if (>= (+ (byte-buffer-position buffer) num-bytes) (byte-buffer-end buffer))
      (error 'byte-buffer-overflow-error :buffer buffer :index (+ (byte-buffer-position buffer)
                                                                  num-bytes)))
  (ptr-copy src (byte-buffer-offset buffer) num-bytes)
  (set-byte-buffer-position buffer (+ (byte-buffer-position buffer)
                                      num-bytes))
  buffer)

(defun byte-buffer-read (buffer)
  (let ((b (ptr-read-byte (+ (byte-buffer-data buffer)
                             (byte-buffer-position buffer)))))
    (set-byte-buffer-position buffer (+ (byte-buffer-position buffer) 1))
    b))


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

(repl-defstruct compiler-output
  ((code-segment :type 'byte-buffer)
   (string-segment :type 'byte-buffer)
   (symbols :type 'symbol-index)))

(defun compiler-output-init (compiler buffer buffer-size)
  (let ((segment-size (ceiling (/ buffer-size 9))))
    (byte-buffer-init (compiler-output-code-segment compiler) (+ buffer (* segment-size 1)) segment-size)
    (byte-buffer-init (compiler-output-string-segment compiler) (+ buffer (* segment-size 5)) segment-size)
    (symbol-index-init (compiler-output-symbols compiler) segment-size (+ buffer (* segment-size 8)))))

(defun compiler-output-define (compiler symbol)
  (symbol-index-define (compiler-output-symbols compiler) symbol)
  compiler)

(defun compiler-output-symbols-buffer (compiler)
  (symbol-index-buffer (compiler-output-symbols compiler)))

(defun compiler-output-symbol-offset (compiler symbol)
  (symbol-index-offset (compiler-symbol-index compiler) symbol))

(defun compiler-output-symbols-next-offset (compiler)
  (symbol-index-next-offset (compiler-output-symbols compiler)))

(defun compiler-output-code-segment-buffer (compiler)
  (byte-buffer-data (compiler-output-code-segment compiler)))

(defun compiler-output-code-segment-position (compiler)
  (byte-buffer-position (compiler-output-code-segment compiler)))

(defun compiler-output-code-segment-offset (compiler)
  (byte-buffer-offset (compiler-output-code-segment compiler)))

(defun compiler-output-copy-to-code-segment (compiler src num-bytes)
  (byte-buffer-copy (compiler-output-code-segment compiler) src num-bytes)
  compiler)

(defun set-compiler-output-string-segment-data (compiler ptr)
  (set-byte-buffer-data (compiler-output-string-segment compiler) ptr)
  (set-byte-buffer-position (compiler-output-string-segment compiler) 0)
  compiler)

(defun compiler-output-string-segment-data (compiler)
  (byte-buffer-data (compiler-output-string-segment compiler)))

(defun set-compiler-output-string-segment-position (compiler n)
  (set-byte-buffer-position (compiler-output-string-segment compiler) n)
  compiler)

(defun set-compiler-output-string-segment-offset (compiler offset)
  (set-byte-buffer-offset (compiler-output-string-segment compiler) offset)
  compiler)

(defun compiler-output-string-segment-offset (compiler)
  (byte-buffer-offset (compiler-output-string-segment compiler)))

(defun compiler-output-string-segment-end (compiler)
  (+ (byte-buffer-data (compiler-output-string-segment compiler))
     (byte-buffer-position (compiler-output-string-segment compiler))))

(defun compiler-output-copy-to-string-segment (compiler src num-bytes)
  (byte-buffer-copy (compiler-output-string-segment compiler) src num-bytes)
  compiler)
