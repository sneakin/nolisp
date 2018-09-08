;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Memory

(require "type-sizes")
(in-package :repl)

;;; All input comes in through *MEMORY* and any array values like symbols
;;; get written to *MEMORY* as well.
#+:sbcl
(defvar *MEMORY* (make-array (* 256 1024) :element-type '(unsigned-byte 8)))
#+:repl
(defvar *MEMORY* 0)

#+:repl (require "runtime/memory")

(defun null? (c)
  (eq c 0))

#+:sbcl
(defun ptr-read-byte (ptr)
  (aref *MEMORY* ptr))

#+:sbcl
(defun ptr-write-byte (c ptr)
  (setf (aref *MEMORY* ptr) c)
  (+ ptr 1))

#+:sbcl
(defun ptr-read-array (ptr elements &optional (arr (make-array elements :element-type '(unsigned-byte 8))))
  (dotimes (n elements)
    (setf (aref arr n) (ptr-read-byte (+ ptr n))))
  arr)

#+:sbcl
(defun ptr-write-array (ptr array)
  (dotimes (n (length array))
    (ptr-write-byte (aref array n) (+ ptr n))))

(defun ptr-read-short (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)))

#+:sbcl
(defun mask-ulong (byte n)
  (ldb (byte 8 (* 8 byte)) n))

#+:repl
(defun mask-ulong (byte n)
  (logand n (ash #xFF byte)))

(defun ptr-write-short (n ptr)
  (ptr-write-byte (mask-ulong 0 n) ptr)
  (ptr-write-byte (mask-ulong 1 n) (+ 1 ptr)))

#+:sbcl
(defun ptr-read-long (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)
          (ash (ptr-read-byte (+ ptr 2)) 16)
          (ash (ptr-read-byte (+ ptr 3)) 24)))

#+:sbcl
(defun ptr-write-long (n ptr)
  (if (eq n nil) (setf n 0))
  (ptr-write-byte (mask-ulong 0 n) ptr)
  (ptr-write-byte (mask-ulong 1 n) (+ 1 ptr))
  (ptr-write-byte (mask-ulong 2 n) (+ 2 ptr))
  (ptr-write-byte (mask-ulong 3 n) (+ 3 ptr))
  )

#+:sbcl
(defun ptr-write-string (str ptr)
  (dotimes (n (length str))
    (ptr-write-byte (char-code (aref str n)) (+ n ptr)))
  (ptr-write-byte 0 (+ (length str) ptr)))

#+:repl
(defun string-length (str &optional (n 0))
  (if (null? (ptr-read-byte str))
      n
      (string-length (+ str 1) (+ n 1))))

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

#+:repl
(defun ptr-write-string (str ptr)
  (ptr-copy str ptr (string-length ptr)))


#+:sbcl
(defun ptr-read-string (ptr &optional count acc (n 0))
  (let* ((c (ptr-read-byte ptr)))
    (if (or (and count (>= n count)) (null? c))
        acc
        (progn
          (setf acc (concatenate 'string acc (list (code-char c))))
          (ptr-read-string (+ 1 ptr) count acc (+ 1 n))))))

#+:repl
(defun ptr-read-string (ptr &optional count acc (n 0))
  (let* ((c (ptr-read-byte ptr)))
    (if (or (and count (>= n count)) (null? c))
        acc
        (progn
          (ptr-read-string (+ 1 ptr) count (ptr-write-byte c acc) (+ 1 n))))))

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
        (format *standard-output* "find-string-equal ~A ~A~%" str current)
        (if (> (length current) 0)
            (if (string-equal current str)
                stack-start
                (ptr-find-string-equal str (+ 1 stack-start (length current)) stack-end))))))

#+:sbcl
(defun ptr-read-file (path offset)
  (with-open-file (f path
                     :direction :input
                     :external-format :default
                     :element-type '(unsigned-byte 8))
    (ptr-write-byte 0 (read-sequence *memory* f :start offset))))

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

(defun ptr-cons (stack &optional head tail)
  (ptr-write-long tail (ptr-write-long head stack)))

(defun ptr-head (stack)
  (ptr-read-long stack))

(defun ptr-rest (stack)
  (+ stack *SIZEOF_LONG*))

#-:sbcl
(defun pointer-of (needle haystack)
  (let ((h (ptr-read-byte haystack)))
    (cond
      ((null? h) nil)
      ((eq needle h) haystack)
      (t (pointer-of needle (+ 1 haystack))))))

(defun align-bytes (bytes &optional (alignment *SIZEOF_LONG*))
  (* (ceiling (/ bytes alignment)) alignment))

#+:sbcl
(defvar *allocate-next-offset* (- (length *MEMORY*) 1))

#+:sbcl
(defun allocate (bytes)
  (setq *allocate-next-offset* (- *allocate-next-offset* bytes))
  *allocate-next-offset*)

#+:sbcl
(defun unallocate (bytes)
  (setq *allocate-next-offset* (+ *allocate-next-offset* bytes))
  *allocate-next-offset*)

#+:sbcl
(defmacro with-allocation (binding &rest body)
  (let ((bytes (second binding))
        (binding (first binding)))
    `(let* ((,binding (allocate ,bytes))
            (ret (progn ,@body)))
       (unallocate ,bytes)
       ret)))


