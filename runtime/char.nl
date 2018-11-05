;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/logic")
(require "runtime/cmp")
(require "runtime/math")

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

