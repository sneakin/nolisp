;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/bitops")

(defun test-ash ()
  (assert-equal (ash 2 0) 2)
  (assert-equal (ash 2 4) 32)
  (assert-equal (ash 32 -4) 2))

(defun test-logand ()
  (assert-equal (logand #x1234567 #xFF00FF00) #x12004500))

(defun test-logior ()
  (assert-equal (logior #x1 #x10) #x11))

(defun test-suite-bitops ()
  (test-ash)
  (test-logand)
  (test-logior)
  (values 411 900))

(run-test-suite test-suite-bitops)
