;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/itoa")

(defun test-itoa ()
  (with-allocation (out 32)
    (assert (string-equal "0" (itoa 0 out)))
    (assert (string-equal "1" (itoa 1 out)))
    (assert (string-equal "-1" (itoa -1 out)))
    (assert (string-equal "FFFFFFFF" (itoa-unsigned #xFFFFFFFF out 16)))
    (assert (string-equal "11111111111111111111111111111111" (itoa-unsigned #xFFFFFFFF out 2)))
    (assert (string-equal "1234" (itoa 1234 out)))
    (assert (string-equal "-1234" (itoa -1234 out)))
    (assert (string-equal "1234" (itoa #x1234 out 16)))
    (assert (string-equal "-1234" (itoa #x-1234 out 16)))
    ))

(run-test-suite test-itoa/0)
