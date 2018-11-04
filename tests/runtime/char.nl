;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/string")

(defun test-downcase-char ()
  (assert-eq (downcase-char #\h) #\h)
  (assert-eq (downcase-char #\H) #\h)
  (assert-eq (downcase-char #\1) #\1)
  )

(defun test-upcase-char ()
  (assert-eq (upcase-char #\H) #\H)
  (assert-eq (upcase-char #\h) #\H)
  (assert-eq (upcase-char #\1) #\1)
  )

(defun test-char ()
  (test-downcase-char)
  (test-upcase-char)
  )

(run-test-suite test-char/0)
