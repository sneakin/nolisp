;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;;
;;; Test functions
;;;

(require "compiler")
(require "outputter")
(require "file-outputter")
(require "assert")

(in-package :repl)

(defun assert-read-token (offset kind value new-offset token-offset &optional new-token-offset)
  (multiple-value-bind (k v o to) (read-token offset token-offset)
    (format *standard-output* "~A ~A ~A ~A ~A ~A~%" k v t to value (if (or (eq k 'symbol) (eq k 'string)) (symbol-string value)))
    (assert (eq k kind))
    (assert (eq v value))
    (assert (eq o new-offset))
    (if new-token-offset (assert (eq to new-token-offset)))))

(defun assert-read-symbol (offset value new-offset token-offset)
  (setf *TOKEN-SEGMENT* token-offset)
  (assert-read-token offset 'symbol token-offset new-offset token-offset (+ 1 token-offset (length value)))
  (assert (string= (ptr-read-string token-offset) value)))

(defun assert-read-character (offset value new-offset token-offset)
  (setf *TOKEN-SEGMENT* token-offset)
  (assert-read-token offset 'character (char-code value) new-offset token-offset token-offset))

(defun assert-read-string (offset value new-offset token-offset &optional (len (+ 2 (length value))))
  (setf *TOKEN-SEGMENT* token-offset)
  (assert-read-token offset 'string token-offset new-offset token-offset (+ 1 token-offset len))
  (assert (string= (ptr-read-string token-offset) value)))

(defun assert-read-integer (offset value new-offset token-offset)
  (assert-read-token offset 'integer value new-offset token-offset token-offset))

(defun assert-read-float (offset value new-offset token-offset)
  (assert-read-token offset 'float value new-offset token-offset token-offset))

(defun assert-read-special (offset char new-offset token-offset)
  (assert-read-token offset 'special (char-code char) new-offset token-offset token-offset))

(defun test-read-token ()
  (ptr-write-string "123" 0)
  (assert-read-integer 0 123 3 1024)
  (ptr-write-string "-123.45" 0)
  (assert-read-float 0 -123.45 7 1024)
  (ptr-write-string (format nil ";; a comment~%123") 0)
  (assert-read-integer 0 123 16 1024)
  (ptr-write-string "hello-world" 0)
  (assert-read-symbol 0 "hello-world" 11 1024)
  (ptr-write-string "-hello" 0)
  (assert-read-symbol 0 "-hello" 6 1024)
  (ptr-write-string "   \"boo \\\"who\"" 0)
  (assert-read-string 0 "boo \"who" 14 1024 8)
  (ptr-write-string "#\\space" 0)
  (assert-read-character 0 #\space 7 1024)
  (ptr-write-string "#\\newline" 0)
  (assert-read-character 0 #\newline 9 1024)
  (ptr-write-string "#\\A" 0)
  (assert-read-character 0 #\A 3 1024)
  (ptr-write-string "#\\ " 0)
  (assert-read-character 0 #\space 3 1024)
  (ptr-write-string "#x32" 0)
  (assert-read-integer 0 #x32 4 1024)
  (ptr-write-string "#XFFFF" 0)
  (assert-read-integer 0 #xFFFF 6 1024)

  (ptr-write-string "hello (world) 123 -34 + +45 +hello #\\space 3.14 -" 0)
  (assert-read-symbol 0 "hello" 5 1024)
  (assert-read-special 5 #\( 7 1024)
  (assert-read-symbol 7 "world" 12 (+ 1024 6))
  (assert-read-special 12 #\) 13 (+ 1024 6))
  (assert-read-integer 13 123 17 (+ 1024 6))
  (assert-read-integer 17 -34 21 (+ 1024 6))
  (assert-read-symbol 21 "+" 23 (+ 1024 6 2))
  (assert-read-integer 23 45 27 (+ 1024 6 2))
  (assert-read-symbol 27 "+hello" 34 (+ 1024 6 2 7))
  (assert-read-character 34 #\space 42 (+ 1024 6 2 7 6))
  (assert-read-float 42 3.14 47 (+ 1024 6 2 7 6 8))
  (assert-read-symbol 47 "-" 49 (+ 1024 6 2 7 6 8 5))
  )

(defun test-read-self ()
  ;; read this source
  ;; see if read never errors
  )

(defun print-string (start)
  (let* ((str (ptr-read-string start))
         (len (length str)))
    (if (> len 0)
        (progn
          (format *standard-output* "~A  ~A~%" start str)
          (+ 1 len)))))

(defun print-strings (start size)
  (if (> size 0)
      (let ((len (print-string start)))
        (if len
            (print-strings (+ start len) (- size len))))))

(defun debug-compile (o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
      (repl-compile o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel o-toplevel)
    (format *standard-output* "~A ~A ~A ~A ~A ~A~%" offset code-segment asm-stack token-offset env toplevel)
    (print 'Input)
    (print-string o-offset)
    (print (ptr-read-array o-offset (- offset o-offset)))
    (print 'Code-Segment)
    (print (ptr-read-array o-code-segment (- code-segment o-code-segment)))
    (print 'Asm-Stack)
    (print (ptr-read-array o-asm-stack (- asm-stack o-asm-stack)))
    (print 'Strings)
    (print-strings o-token-offset (- token-offset o-token-offset))
    (print (ptr-read-array o-token-offset (- token-offset o-token-offset)))
    (print 'Env)
    (print (ptr-read-array env-start (- env env-start)))
    (print 'Toplevel)
    (print (ptr-read-array o-toplevel (- toplevel o-toplevel)))))

(defun test-compile ()
  (ptr-write-string "(print 'value (+ x 1 2 (* 3 4 (square x)) 5))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-quote ()
  (ptr-write-string "(quote value)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-let ()
  (ptr-write-string "(let ((+ 0) (* 0) (square 0) (print 0)
                           (x (+ 1 2))
                           (y (* 3 4 (square x))))
                       (print x)
                       (print y)
                       (print (+ x y)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-local ()
  (ptr-write-string "(let ((x 0)
                           (y 0))
                       (set x (+ 1 123))
                       123.45
                       (set y 456)
                       (let ((z 1))
                         (set z 2)
                         (set x 456)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-lambda ()
  (ptr-write-string "(lambda (x y)
                       (print x y)
                       (set x (+ 1 123))
                       (set y 456))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-named-lambda ()
  (ptr-write-string "(lambda f (x y)
                       (print y x)
                       (set x (+ 1 123))
                       (set y 456)
                       (f (+ 1 x) y))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-lambda ()
  (ptr-write-string "(set f (lambda (n)
                       n
                       (f n)))
                     (set g (lambda () (f 10)))
                     (g (f 123))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-global ()
  (ptr-write-string "(set x 123)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-if-2 ()
  (ptr-write-string "(let ((x 1) (t 1) (nil 0)) (if x t nil))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-if ()
  (ptr-write-string "(if 0 1 (progn 1 2 3))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-asm ()
  (ptr-write-string "(asm (push 0))
                     (asm (load 2) 1234 (push 2) (mov 2 0))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-values ()
  (ptr-write-string "(apply-values (lambda (a b c) a) (values 1 2 3))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-mvb ()
  (ptr-write-string "(multiple-value-bind (a b) (values 8 9) a)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-cond ()
  (ptr-write-string "(set t 't) (set eq 'eq) (set x 2)
                     (cond
                      ((eq x 1) 123)
                      ((eq x 2) 456)
                      (t 789))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-conditional ()
  (ptr-write-string "#+:sbcl (set x 3) #-(or :sbcl :repl) (set y 3) #+(and repl x86) (set x 4)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-write-to-array ()
  (write-to-array 6000 1000 1050 2000 2006 3000 3009 4000 4004 5000 5004))

(defun test-write (&optional (path "test.out"))
  (ptr-write-string "(set f (lambda (x) (if x (values 1 2 3 4) 255))) (f (f 123)) (f 0) (apply-values f (values 8 9))" 0)
  (compile-to-file path 6000 0 1000 2000 3000 4000 4004 5000))

(defun test ()
  (test-read-token))
