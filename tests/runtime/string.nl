;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/string")

(defun test-string-length ()
  (assert-equal (length nil) 0)
  (assert-equal (length "Hello") 5)
  )

(defun test-string-equal ()
  (assert-equal (string-equal nil nil) t)
  (assert-equal (string-equal "" "") t)
  (assert-equal (string-equal "Hello" "Hello") t)
  (assert-equal (string-equal "Hello" "world") nil)
  (assert-equal (string-equal "Hello" "Hello world") nil)
  (assert-equal (string-equal "Hello world" "Hello") nil))

(defun test-string ()
  (test-string-length)
  (test-string-equal))

(run-test-suite test-string/0)
