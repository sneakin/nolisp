;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/bitops")

(defun test-ash ()
  (assert-equal (ash 2 0) 2)
  (assert-equal (ash 2 4) 32)
  (assert-equal (ash 32 -4) 2)
  )

(defun test-logand ()
  (assert-equal (logand #x12345678 #xFF00FF00) #x12005600))

(defun test-logior ()
  (assert-not-equal (logior #x100 #x100) #x200)
  (assert-equal (logior #x100 #x210) #x310))

(defun test-suite-bitops ()
  (test-ash)
  (test-logand)
  (test-logior)
  )

(run-test-suite test-suite-bitops/0)
