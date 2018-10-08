;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/math")
(require "runtime/memory")

(defun test-ptr-long ()
  (with-allocation (a 128)
                   (assert-equal (ptr-write-long 1234 a) (+ a 4))
                   (assert-equal (ptr-read-long a) 1234)
                   (assert-equal (ptr-write-long -1234 a) (+ a 4))
                   (assert-equal (ptr-read-long a) -1234)))

(defun test-ptr-ulong ()
  (with-allocation (a 128)
                   (assert-equal (ptr-write-long #xFFFF a) (+ a 4))
                   (assert-equal (ptr-read-long a) #xFFFF)))

(defun test-ptr-byte ()
  (with-allocation (a 128)
                   (assert-equal (ptr-write-byte 123 a) (+ a 1))
                   (assert-equal (ptr-read-byte a) 123)))

(defun test-ptr-copy-up ()
  )

(defun test-ptr-copy-down ()
  )

(defun test-ptr-set ()
  )

(defun test-ptr-zero ()
  )

(defun test-memory ()
  (test-ptr-long)
  (test-ptr-byte)
  (test-ptr-copy-up)
  (test-ptr-copy-down)
  (test-ptr-set)
  (test-ptr-zero)
  )

(run-test-suite test-memory/0)
