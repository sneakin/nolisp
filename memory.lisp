;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Memory

(require "type-sizes")
#+:repl (require "runtime/number")

#-:repl (in-package :repl)

;;; All input comes in through *MEMORY* and any array values like symbols
;;; get written to *MEMORY* as well.
#-:repl
(defvar *MEMORY* (make-array (* 1024 1024 8) :element-type '(unsigned-byte 8)))
#+:repl
(defvar *MEMORY* 0)

#+:repl (require "runtime/null")
#+:repl (require "runtime/memory")
#+:repl (require "runtime/math")
#+:repl (require "runtime/string")
#+:repl (require "runtime/error")

#-:repl
(defun zero? (c)
  (eq c 0))

#-:repl
(defun ptr-read-byte (ptr)
  (if (< ptr (length *MEMORY*))
      (aref *MEMORY* ptr)))

#-:repl
(defun ptr-read-ubyte (ptr)
  (ptr-read-byte ptr))

#-:repl
(defun ptr-write-byte (c ptr)
  (setf (aref *MEMORY* ptr) c)
  (+ ptr 1))

#-:repl
(defun ptr-write-ubyte (c ptr)
  (ptr-write-byte c ptr))

#-:repl
(defun ptr-write-char (c ptr)
  (ptr-write-byte (char-code c) ptr))

#-:repl
(defun ptr-read-array (ptr elements &optional (arr (make-array elements :element-type '(unsigned-byte 8))))
  (dotimes (n elements)
    (setf (aref arr n) (ptr-read-byte (+ ptr n))))
  arr)

#-:repl
(defun ptr-write-array (ptr array)
  (dotimes (n (length array))
    (ptr-write-byte (aref array n) (+ ptr n))))

#-:repl
(defun ptr-read-ushort (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)))

#-:repl
(defun ptr-read-short (ptr)
  (let ((n (ptr-read-ushort ptr)))
    (if (eq 0 (logand n #x8000))
        n
        (- (- n #xFFFF) 1))))


#-:repl
(defun mask-ulong (byte n)
  (ldb (byte 8 (* 8 byte)) n))

#+:repl
(defun mask-ulong (byte n)
  (logand n (ash #xFF byte)))

#-:repl
(defun ptr-write-short (n ptr)
  (ptr-write-byte (mask-ulong 0 n) ptr)
  (ptr-write-byte (mask-ulong 1 n) (+ 1 ptr)))

#-:repl
(defun ptr-write-ushort (n ptr)
  (ptr-write-short n ptr))

#-:repl
(defun make-ushort (a b)
  (logior a
          (ash b 8)))

#-:repl
(defun make-ulong (a b c d)
  (logior a
          (ash b 8)
          (ash c 16)
          (ash d 24)))

#-:repl
(defun make-ulong64 (a b c d e f g h)
  (logior a
          (ash b 8)
          (ash c 16)
          (ash d 24)
          (ash e 32)
          (ash f 40)
          (ash g 48)
          (ash h 56)))

#-:repl
(defun ptr-read-ulong (ptr)
  (make-ulong (ptr-read-byte ptr)
              (ptr-read-byte (+ ptr 1))
              (ptr-read-byte (+ ptr 2))
              (ptr-read-byte (+ ptr 3))))

#-:repl
(defun ptr-read-ulong64 (ptr)
  (make-ulong64 (ptr-read-byte ptr)
                (ptr-read-byte (+ ptr 1))
                (ptr-read-byte (+ ptr 2))
                (ptr-read-byte (+ ptr 3))
                (ptr-read-byte (+ ptr 4))
                (ptr-read-byte (+ ptr 5))
                (ptr-read-byte (+ ptr 6))
                (ptr-read-byte (+ ptr 7))))

#-:repl
(defun ptr-read-long (ptr)
  (let ((n (ptr-read-ulong ptr)))
    (if (eq 0 (logand n #x80000000))
        n
        (- (- n #xFFFFFFFF) 1))))

#-:repl
(defun ptr-write-long (n ptr)
  (if (eq n nil) (setf n 0))
  (ptr-write-byte (mask-ulong 0 n) ptr)
  (ptr-write-byte (mask-ulong 1 n) (+ 1 ptr))
  (ptr-write-byte (mask-ulong 2 n) (+ 2 ptr))
  (ptr-write-byte (mask-ulong 3 n) (+ 3 ptr)))

#-:repl
(defun ptr-write-ulong (n ptr)
  (ptr-write-long n ptr))

#-:repl
(defun ptr-write-long64 (n ptr)
  (if (eq n nil) (setf n 0))
  (ptr-write-byte (mask-ulong 0 n) ptr)
  (ptr-write-byte (mask-ulong 1 n) (+ 1 ptr))
  (ptr-write-byte (mask-ulong 2 n) (+ 2 ptr))
  (ptr-write-byte (mask-ulong 3 n) (+ 3 ptr))
  (ptr-write-byte (mask-ulong 4 n) (+ 4 ptr))
  (ptr-write-byte (mask-ulong 5 n) (+ 5 ptr))
  (ptr-write-byte (mask-ulong 6 n) (+ 6 ptr))
  (ptr-write-byte (mask-ulong 7 n) (+ 7 ptr)))

#-:repl
(defun ptr-write-ulong64 (n ptr)
  (ptr-write-long64 n ptr))

#-:repl
(defun ptr-write-ptr (n ptr)
  (ptr-write-ulong n ptr))

#-:repl
(defun ptr-write-pointer (n ptr)
  (ptr-write-ulong n ptr))

#-:repl
(defun ptr-read-ptr (ptr)
  (ptr-read-ulong ptr))

#-:repl
(defun ptr-read-pointer (ptr)
  (ptr-read-ulong ptr))

#-:repl
(defun ptr-write-string (str ptr &optional (limit (if (not (numberp str))
                                                      (length str))))
  (if (numberp str)
      (ptr-write-string (ptr-read-string str) ptr)
      (progn
        (dotimes (n (min limit (length str)))
          (ptr-write-byte (char-code (aref str n)) (+ n ptr)))
        (ptr-write-byte 0 (+ (min limit (length str)) ptr)))))

#+:repl
(defun string-length (str &optional (n 0))
  (if (zero? (ptr-read-byte str))
      n
      (string-length (+ str 1) (+ n 1))))

(defun ptr-copy (src dest count)
  (if (> count *SIZEOF_LONG*)
      (progn
        (ptr-write-long (ptr-read-ulong src) dest)
        (ptr-copy (+ src *SIZEOF_LONG*) (+ dest *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
      (if (> count 0)
          (progn
            (ptr-write-byte (ptr-read-byte src) dest)
            (ptr-copy (+ src 1) (+ dest 1) (- count 1)))
          dest)))

#-:repl
(defun ptr-read-string-loop (ptr count acc n)
  (let* ((c (ptr-read-byte ptr)))
    (if (or (and count (>= n count)) (zero? c))
        acc
        (progn
          (setf acc (concatenate 'string acc (list (code-char c))))
          (ptr-read-string-loop (+ 1 ptr) count acc (+ 1 n))))))

#-:repl
(defun ptr-read-string (ptr &optional count acc (n 0))
  (if (stringp ptr)
      ptr
      (ptr-read-string-loop ptr count "" n)))

#+:repl
(defun ptr-read-string (ptr &optional count acc n)
  ptr)

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
        (if (and (> (length current) 0)
                 (string= current str))
            stack-start
            (ptr-find-string= str (+ 1 stack-start (length current)) stack-end)))))

(defun ptr-find-string-equal (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (and (> (length current) 0)
                 (string-equal current str))
            stack-start
            (ptr-find-string-equal str (+ 1 stack-start (length current)) stack-end)))))

#-:repl
(defun ptr-read-file (path offset)
  (with-open-file (f path
                     :direction :input
                     :external-format :default
                     :element-type '(unsigned-byte 8))
    (ptr-write-byte 0 (read-sequence *memory* f :start offset))))

#+:repl
(defun ptr-read-file (path offset)
  (error 'not-implemented-error))

(defun ptr-write-quad (value offset)
  (ptr-write-long (make-ulong value value value value) offset))

(defun ptr-set (offset count &optional (value 0) (quad (make-ulong value value value value)))
  (if (> count 4)
      (ptr-set (ptr-write-long quad offset)
               (- count *SIZEOF_LONG*)
               value
               quad)
      (if (> count 0)
          (progn
            (ptr-write-byte value offset)
            (ptr-set (+ offset 1) (- count 1) value quad))
          offset)))

(defun ptr-zero (offset count)
  (ptr-set offset count 0))

#-:repl
(defun ptr-write (value ptr)
  (cond
    ((floatp value) (ptr-write-float value ptr))
    ((stringp value) (ptr-write-string value ptr))
    ((symbolp value) (ptr-write-string (symbol-string value) ptr))
    (t (ptr-write-long value ptr))))

#+:repl
(defun ptr-write (value ptr)
  (ptr-write-long value ptr))

(defun ptr-cons (stack &optional head tail)
  (ptr-write tail (ptr-write head stack)))

(defun ptr-head (stack)
  (if stack (ptr-read-ulong stack)))

(defun ptr-rest (stack)
  (if stack
      (let ((addr (+ stack *SIZEOF_LONG*)))
        (if (zero? (ptr-head addr))
            nil
            addr))))

(defun ptr-tail (stack)
  (if stack
      (let ((addr (ptr-rest stack)))
        (if addr (ptr-read-ulong addr)))))

(defun ptr-tail-float (stack)
  (if stack
      (let ((addr (ptr-rest stack)))
        (if addr (ptr-read-float addr)))))

(defun pointer-of (needle haystack)
  (let ((h (ptr-read-byte haystack)))
    (cond
      ((zero? h) nil)
      ((eq needle h) haystack)
      (t (pointer-of needle (+ 1 haystack))))))

#-:repl
(defun align-bytes (bytes &optional (alignment *SIZEOF_LONG*))
  (if (and (> bytes 0)
           (> alignment 0))
      (* (ceiling (/ bytes alignment)) alignment)
      0))

#+:repl
(defun align-bytes (bytes &optional (alignment *SIZEOF_LONG*))
  (if (and (> bytes 0)
           (> alignment 0))
      (* (+ 1 (/ bytes alignment)) alignment)
      0))

#-:repl
(defvar *allocate-next-offset* (- (length *MEMORY*) 1))

#-:repl
(defun allocate (bytes)
  (setq *allocate-next-offset* (- *allocate-next-offset* bytes))
  *allocate-next-offset*)

#-:repl
(defun unallocate (bytes)
  (setq *allocate-next-offset* (+ *allocate-next-offset* bytes))
  *allocate-next-offset*)

#-:repl
(defmacro with-allocation (binding &rest body)
  (let ((bytes (second binding))
        (binding (first binding)))
    `(unwind-protect
         (let* ((,binding (allocate ,bytes)))
           ,@body)
       (unallocate ,bytes)
       nil)))
