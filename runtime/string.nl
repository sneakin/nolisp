;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun length (str &optional (n 0))
  (if (eq (ptr-read-byte str) 0)
      n
      (length (+ str 1) (+ n 1))))

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

(defun char-code (code)
  code)

(defun downcase (c)
  c)

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
