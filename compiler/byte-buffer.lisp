;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")

(in-package :repl)

#+:repl (require "bootstrap/byte-buffer")
#-:repl (require "runtime/defstruct")
#-:repl
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
  (let ((dest (byte-buffer-offset buffer)))
    (ptr-copy src dest num-bytes)
    (set-byte-buffer-position buffer (+ (byte-buffer-position buffer)
                                        num-bytes))
    dest))

(defun byte-buffer-copy-string (buffer src)
  (let ((num-bytes (+ (length src) 1)))
    (if (>= (+ (byte-buffer-position buffer) num-bytes) (byte-buffer-end buffer))
        (error 'byte-buffer-overflow-error :buffer buffer :index (+ (byte-buffer-position buffer)
                                                                    num-bytes)))
    (let ((dest (byte-buffer-offset buffer)))
      (ptr-write-string src dest)
      (set-byte-buffer-position buffer (+ (byte-buffer-position buffer)
                                          num-bytes))
      dest)))

(defun byte-buffer-read (buffer)
  (let ((b (ptr-read-byte (+ (byte-buffer-data buffer)
                             (byte-buffer-position buffer)))))
    (set-byte-buffer-position buffer (+ (byte-buffer-position buffer) 1))
    b))

