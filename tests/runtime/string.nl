;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "assert")
(require "runtime/string")

(defun test-string-access ()
  (assert-equal (ptr-read-ubyte "Hello") #\H)
  (let ((str "World"))
    (assert-equal (ptr-read-ubyte str) #\W)
    (assert-equal (ptr-read-ubyte (+ str 1)) #\o)))

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

(defun test-downcase ()
  (assert-equal (downcase "HELLO") "hello")
  (assert-equal (downcase "HELLO\nw0rld!?") "hello\nw0rld!?"))

(defun test-upcase ()
  (assert-equal (upcase "hello") "HELLO")
  (assert-equal (upcase "HELLO\nw0rld!?") "HELLO\nW0RLD!?"))

(defun test-string-length ()
  (assert-equal (length nil) 0)
  (assert-equal (length "") 0)
  (assert-equal (length "Hello") 5)
  (assert-equal (length "Hello world") 11)
  (assert-equal (length "Hello\nworld") 11)
  )

(defun test-string= ()
  (assert-equal (string= nil nil) t)
  (assert-equal (string= nil "Hey") nil)
  (assert-equal (string= "Hey" nil) nil)
  (assert-equal (string-equal "" "") t)
  (assert-equal (string-equal "Hello" "Hello") t)
  (assert-equal (string-equal "Hello" "world") nil)
  (assert-equal (string-equal "Hello" "Hello world") nil)
  (assert-equal (string-equal "Hello world" "Hello") nil)
  (assert-equal (string-equal "Hello\nworld" "Hello world") nil)
  )

(defun test-string-equal ()
  (assert-equal (string-equal nil nil) t)
  (assert-equal (string-equal nil "Hey") nil)
  (assert-equal (string-equal "Hey" nil) nil)
  (assert-equal (string-equal "" "") t)
  (assert-equal (string-equal "Hello" "Hello") t)
  (assert-equal (string-equal "Hello" "heLLo") t)
  (assert-equal (string-equal "Hello" "world") nil)
  (assert-equal (string-equal "Hello" "Hello world") nil)
  (assert-equal (string-equal "Hello world" "Hello") nil)
  (assert-equal (string-equal "Hello\nworld" "Hello\nWorld") t)
  (assert-equal (string-equal "Hello\nworld" "Hello World") nil)
  )


(defun test-string ()
  (test-string-access)
  (test-downcase-char)
  (test-upcase-char)
  (test-string-length)
  (test-string=)
  (test-string-equal)
  )

(run-test-suite test-string/0)
