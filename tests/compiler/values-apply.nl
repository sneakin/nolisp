;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/halt")
(require "runtime/eq")

(defun fn (a b c)
  (assert-equal a 1)
  (assert-equal b 2)
  (assert-equal c 3))

(defun test-apply-values ()
  (apply-values fn/3 3 (values 1 2 3))
  ;; (apply-values (lambda (a b)
  ;;                 (assert-equal a 100)
  ;;                 (assert-equal b 200))
  ;;               2
  ;;               (values 100 200))
  )

(test-apply-values)
(values 411 900)
(halt)
