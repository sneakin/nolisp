;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/sequence")

(defun length (str &optional (n 0))
  (if (eq (ptr-read-byte str) 0)
      n
      (length (+ str 1) (+ n 1))))

(defun string-aref (str n)
  (ptr-read-byte (+ str n)))

(defun string-equal (a b)
  (let* ((ac (ptr-read-byte a))
         (bc (ptr-read-byte b)))
    (cond
      ((and (eq ac 0) (eq bc 0))
       t)
      ((or (eq ac 0) (eq bc 0))
       nil)
      ((eq ac bc)
       (string-equal (+ a 1) (+ b 1)))
      (t nil))
    )
  )

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
      (+ (- c (char-code #\a)) (+ c (char-code #\A)))
      c))

(defun downcase-char (c)
  (if (is-upcase? c)
      (+ (- c (char-code #\A)) (+ c (char-code #\a)))
      c))

(defun string-downcase (str &optional (starting str))
  (let ((c (ptr-read-byte str)))
    (if (zero? c)
        str
        (progn
          (if (is-downcase? c)
              (ptr-write-byte (downcase-char c) str))
          (string-downcase (+ str 1) (or starting str))))))

(defun downcase (c)
  (string-downcase c))

(defun string-upcase (str &optional (starting str))
  (let ((c (ptr-read-byte str)))
    (if (zero? c)
        str
        (progn
          (if (is-upcase? c)
              (ptr-write-byte (upcase-char c) str))
          (string-upcase (+ str 1) (or starting str))))))

(defun string-equal (a b)
  (let* ((ac (downcase (ptr-read-byte a)))
         (bc (downcase (ptr-read-byte b))))
    (cond
      ((and (eq ac 0) (eq bc 0))
       t)
      ((or (eq ac 0) (eq bc 0))
       nil)
      ((eq ac bc)
       (string-equal (+ a 1) (+ b 1)))
      (t nil))
    )
  )

(defun string= (a b)
  (let* ((ac (downcase (ptr-read-byte a)))
         (bc (downcase (ptr-read-byte b))))
    (cond
      ((and (eq ac 0) (eq bc 0))
       t)
      ((or (eq ac 0) (eq bc 0))
       nil)
      ((eq ac bc)
       (string-equal (+ a 1) (+ b 1)))
      (t nil))
    )
)

(defun itoa (n &optional (base 10))
  n)
