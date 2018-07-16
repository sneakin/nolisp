;;; -*- mode: Lisp; coding: utf-8-unix -*-
;(require "pgen-comp")

(use-package :pgen)

(defun print-args (&rest args)
  (format *standard-output* "Args: ~A~%~%" args)
  (first args))

(defvar *tokens* '())
(defun push-token (a)
  (setf *tokens* (cons a *tokens*))
  a)
(defun digit-value (c) (- (if (characterp c)
                              (char-code c)
                            c)
                          (char-code #\0)))
(defun digit-list-to-number (lst &optional neg (n 0) (base 10))
  (if lst
      (digit-list-to-number (rest lst) neg (+ (* n base) (first lst)))
    (if neg
        (- n)
      n)))


(define-parser test-parser
  ((top (+ root))
   (root (or abc ab ac number space) @(print-args 'root $1))
   (space (or #\space #\newline))
   (abc #\a #\b #\c $(push-token 'abc) @(print-args 'abc $0 $1))
   (ab #\a #\b @(print-args 'on-ab $0 $1) $(push-token 'ab) @(print-args 'ab $0 $1))
   (ac #\a #\c $(push-token 'ac) @(print-args 'ac $0 $1))
   (number (? #\-)
           (+ (range #\0 #\9) >(digit-value $1))
           >(push-token (digit-list-to-number $2 (not (eq $1 T)))))))

(define-parser test-parse-range
  ((top (? #\-) (+ (range #\0 #\9)) @(print-args $0 $1))))

(define-parser test-parse-or
  ((top (or alpha digit)  > (print-args $0))
   (alpha #\a @ (print-args 'alpha $0 $1))
   (digit (? #\-) (range #\0 #\9) @ (print-args 'digit $0 $1))))

(define-parser test-parse-one-or-more
  ((top (? #\- #\space) (+ #\a) @ (print-args 'top $0))))

(define-parser test-parse-one-or-more-2
  ((top (? #\-) (+ #\a) @ (print-args 'top $0))))

(define-parser test-parse-zero-or-one-2
  ((top (? #\- #\space) #\a @ (print-args 'top $0))))

(define-parser test-parse-zero-or-more
  ((top (? #\- #\space) #\. (* #\a) @ (print-args 'top $0))))

(define-parser test-parse-zero-or-one
  ((top (? #\a) @ (print-args 'top $0))))

(define-parser test-parse-exact
  ((top #\a #\b  > (print-args 'top> $0 $1 $2) @ (print-args 'top@ $0 $1))))

(define-parser test-parse-number
    "Parses an integer from a string."
    ((integer (? #\-) (+ digit) > (digit-list-to-number (mapcar #'first $2) (not (eq $1 T))))
     (digit (range #\0 #\9) > (- $1 (char-code #\0)) @ (print-args $1))))

(define-parser test-parse-number-2
    ((digit (range #\0 #\9) > (list (- $1 (char-code #\0))) #\. > (print-args $0))))

(define-parser test-parse-zero-or-one
    ((top #\b any (+ #\a)
          @ (print-args $0))))
