;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "globals")
(require "runtime/eq")
(require "runtime/cmp")
(require "runtime/char")
(require "runtime/bitops")

(defun numberp (obj)
  t)

(defun = (a b)
  (eq a b))

(defun max (a b)
  (if (> a b)
      a
      b))

(defun min (a b)
  (if (< a b)
      a
      b))

(defun make-ulong (&optional (a 0) (b 0) (c 0) (d 255))
  (logior a (logior (ash b 8) (logior (ash c 16) (ash d 24)))))

(defun char-digit (n &optional (base *output-base*))
  (if (>=-unsigned n base)
      (char-digit (mod-unsigned n base) base)
      (if (<-unsigned n 10)
          (code-char (+ (char-code #\0) n))
          (if (<-unsigned n 36)
              (code-char (+ (char-code #\A) (- n 10)))
              (code-char (+ (char-code #\a) (- n 36)))))))

(defun count-digits-guess (n &optional (base *output-base*))
  (floori (log-unsigned n base)))

(defun count-digits-brute (n &optional (base *output-base*) (count 0))
  (if (>-unsigned n 1)
      (count-digits-brute (/-unsigned n base) base (+ count 1))
      count))

(defun count-digits-guesser (n &optional (base *output-base*) (count 0))
  (let ((guess (count-digits-guess n base))
        (guess-value (expt-unsigned base guess))
        (last-digit (/-unsigned n guess-value)))
    (cond
      ((>=-unsigned last-digit base) (count-digits-brute (mod-unsigned n guess-value) base guess))
      ((>=-unsigned last-digit 1) guess)
      (t (- guess 1)))))

(defun count-digits (n &optional (base *output-base*))
  (count-digits-guesser n base))

(defun max-digit-value (n &optional (base *output-base*))
  (expt-unsigned base (count-digits n base)))
