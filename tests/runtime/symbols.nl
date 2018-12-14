;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "symbol")
(require "runtime/string")

(defun test-symbol-name ()
  (assert (string-equal (symbol-name 'hello) (symbol-name 'hello)))
  (assert (not (string-equal (symbol-name 'hello) (symbol-name 'world))))
  (assert (string-equal "hello" (symbol-name 'hello)))
  (assert (string= "hello" (symbol-name 'HELLO)))
  (assert (not (string= "HELLO" (symbol-name 'hello))))
  (assert (string-equal (symbol-name 'hello) (symbol-name :hello))))

(defun test-symbol-keyword ()
  (assert (keyword? :thekey))
  (assert (not (keyword? 'thekey)))
  (assert (not (keyword? 123)))
  )

(defun test-symbol-intern ()
  (assert-equal (intern "hello") 'hello)
  (assert-equal (intern ":hello") :hello)
  (assert-equal (intern 'hello) 'hello)
  (assert-equal (intern :hello) :hello))

(defun test-suite-symbols ()
  (test-symbol-name)
  (test-symbol-keyword)
  (test-symbol-intern)
  (values 411 900))

(test-suite-symbols)
