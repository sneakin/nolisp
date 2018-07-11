;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Memory

(require "null")
(require "type-sizes")

(in-package :repl)

;;; All input comes in through *MEMORY* and any array values like symbols
;;; get written to *MEMORY* as well.
(defvar *MEMORY* (make-array (* 256 1024) :element-type '(unsigned-byte 8)))

(defun ptr-read-byte (ptr)
  (aref *MEMORY* ptr))

(defun ptr-write-byte (c ptr)
  (setf (aref *MEMORY* ptr) c)
  (+ ptr 1))

(defun ptr-read-array (ptr elements &optional (arr (make-array elements :element-type '(unsigned-byte 8))))
  (dotimes (n elements)
    (setf (aref arr n) (ptr-read-byte (+ ptr n))))
    arr)

(defun ptr-copy (src dest count)
  (if (> count *SIZEOF_LONG*)
      (progn
        (ptr-write-long (ptr-read-long src) dest)
        (ptr-copy (+ src *SIZEOF_LONG*) (+ dest *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
    (if (> count 0)
        (progn
          (ptr-write-byte (ptr-read-byte src) dest)
          (ptr-copy (+ src 1) (+ dest 1) (- count 1)))
      dest)))

(defun ptr-read-short (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)))

(defun ptr-write-short (n ptr)
  (ptr-write-byte (ldb (byte 8 0) n) ptr)
  (ptr-write-byte (ldb (byte 8 8) n) (+ 1 ptr)))

(defun ptr-read-long (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)
          (ash (ptr-read-byte (+ ptr 2)) 16)
          (ash (ptr-read-byte (+ ptr 3)) 24)))

(defun ptr-write-long (n ptr)
  (ptr-write-byte (ldb (byte 8 0) n) ptr)
  (ptr-write-byte (ldb (byte 8 8) n) (+ 1 ptr))
  (ptr-write-byte (ldb (byte 8 16) n) (+ 2 ptr))
  (ptr-write-byte (ldb (byte 8 24) n) (+ 3 ptr))
  )

(defun ptr-write-string (str ptr)
  (dotimes (n (length str))
    (ptr-write-byte (char-code (aref str n)) (+ n ptr)))
  (ptr-write-byte 0 (+ (length str) ptr)))

#+:sbcl
(defun ptr-read-string (ptr &optional count acc (n 0))
  (let* ((c (ptr-read-byte ptr)))
    (if (or (and count (>= n count)) (null? c))
        acc
      (progn
        (setf acc (concatenate 'string acc (list (code-char c))))
        (ptr-read-string (+ 1 ptr) count acc (+ 1 n))))))

#+:sbcl
(defun ptr-read-float (ptr)
  (let ((bits (ptr-read-long ptr)))
    (sb-kernel:make-single-float bits)))

#+:sbcl
(defun ptr-write-float (value ptr)
  (ptr-write-long (sb-kernel:single-float-bits value) ptr))

(defun ptr-find-string= (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (> (length current) 0)
            (if (string= current str)
                stack-start
              (ptr-find-string= str (+ 1 stack-start (length current)) stack-end))))))

(defun ptr-find-string-equal (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (> (length current) 0)
            (if (string-equal current str)
                stack-start
              (ptr-find-string-equal str (+ 1 stack-start (length current)) stack-end))))))

(defun ptr-zero (offset count)
  (if (> count 4)
      (progn
        (ptr-write-long 0 offset)
        (ptr-zero (+ offset *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
    (if (> count 0)
        (progn
          (ptr-write-byte 0 offset)
          (ptr-zero (+ offset 1) (- count 1)))
      offset)))

#-:sbcl
(defun pointer-of (needle haystack)
  (let ((h (ptr-read-byte haystack)))
    (cond
     ((null? h) nil)
     ((eq needle h) haystack)
     (t (pointer-of needle (+ 1 haystack))))))

(defun align-bytes (bytes &optional (alignment *SIZEOF_LONG*))
  (* (ceiling (/ bytes alignment)) alignment))
