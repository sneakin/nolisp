;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")
(require "runtime/sequence")
(require "runtime/null")
(require "runtime/math")

(defun length (str &optional (n 0))
  (if str
      (if (eq (ptr-read-ubyte str) 0)
          n
          (length (+ str 1) (+ n 1)))))

(defun string-aref (str n)
  (ptr-read-ubyte (+ str n)))

(defun code-char (char)
  char)

(defun char-code (code)
  code)

(defun is-upcase? (c)
  (and (>= c (char-code #\A))
       (<= c (char-code #\Z))))

(defun is-downcase? (c)
  (and (>= c (char-code #\a))
       (<= c (char-code #\z))))

(defun upcase-char (c)
  (if (is-downcase? c)
      (+ (- c (char-code #\a)) (char-code #\A))
      c))

(defun downcase-char (c)
  (if (is-upcase? c)
      (+ (- c (char-code #\A)) (char-code #\a))
      c))

(defun string-downcase (str &optional (starting str))
  (let ((c (ptr-read-ubyte str)))
    (if (zero? c)
        starting
        (progn
          (if (is-downcase? c)
              (ptr-write-ubyte (downcase-char c) str))
          (string-downcase (+ str 1) (or starting str))))))

(defun downcase (c)
  (string-downcase c))

(defun string-upcase (str &optional (starting str))
  (let ((c (ptr-read-ubyte str)))
    (if (zero? c)
        starting
        (progn
          (if (is-upcase? c)
              (ptr-write-ubyte (upcase-char c) str))
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


(defun itoa (n &optional (base 10))
  n)
