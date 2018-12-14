;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/sequence")
(require "runtime/null")
(require "runtime/char")
(require "runtime/math")
(require "runtime/cmp")
(require "runtime/number")

(defun stringp (str)
  t)

(defun length (str &optional (n 0))
  (if str
      (if (eq (ptr-read-ubyte str) 0)
          n
          (length (+ str 1) (+ n 1)))))

(defun string-aref (str n)
  (ptr-read-ubyte (+ str n)))

(defun string-downcase (str &optional (starting str))
  (let ((c (ptr-read-ubyte str)))
    (if (zero? c)
        starting
        (progn
          (ptr-write-ubyte (downcase-char c) str)
          (string-downcase (+ str 1) (or starting str))))))

(defun downcase (c)
  (string-downcase c))

(defun string-upcase (str &optional (starting str))
  (let ((c (ptr-read-ubyte str)))
    (if (zero? c)
        starting
        (progn
          (ptr-write-ubyte (upcase-char c) str)
          (string-upcase (+ str 1) (or starting str))))))

(defun upcase (c)
  (string-upcase c))

(defun string-equal (a b)
  (let* ((ac (downcase-char (ptr-read-ubyte a)))
         (bc (downcase-char (ptr-read-ubyte b))))
    (if (eq ac bc)
        (if (eq ac 0)
            t
            (string-equal (+ a 1) (+ b 1)))
        nil)))

(defun string= (a b)
  (let* ((ac (ptr-read-ubyte a))
         (bc (ptr-read-ubyte b)))
    (if (eq ac bc)
        (if (eq ac 0)
            t
            (string= (+ a 1) (+ b 1)))
        nil)))

(defun string-position (char str &optional (n 0))
  (let ((c (string-aref str n)))
    (if (eq c char)
        n
        (if (zero? c)
            -1
            (string-position char str (+ n 1))))))

(defun ptr-write-string (str ptr &optional (len (length str)))
  (ptr-write-ubyte 0 (ptr-copy str ptr len)))
