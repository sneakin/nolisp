;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "globals")
(require "runtime/memory")
(require "runtime/sequence")
(require "runtime/eq")
(require "runtime/cmp")
(require "runtime/math")
(require "runtime/number")
(require "runtime/char")
(require "runtime/string")
(require "runtime/convertors/string/globals")
(require "runtime/convertors/integer")

;; fixme need constant access at compile time or variable with-allocations
;;(defconstant *itoa-max-length* 36)

(defun itoa-unsigned-finish (n base output-seq output-start)
  (values output-start (ptr-write-char 0 (ptr-write-char (char-digit n base) output-seq))))

(defun itoa-unsigned-loop (n output-seq
                           &optional
                             (base *output-base*)
                             (divisor (max-digit-value n base))
                             (output-start output-seq))
  (if (>-unsigned divisor 1)
      (let ((next-div (/-unsigned divisor base))
            (digit (floori (/-unsigned n divisor)))
            (remainder (mod-unsigned n divisor)))
        (itoa-unsigned-loop remainder
                            (ptr-write-char (char-digit digit base) output-seq)
                            base
                            next-div
                            output-start))
      (itoa-unsigned-finish n base output-seq output-start)))

(defun itoa-unsigned-digits (n output-seq
                             &optional
                               (base *output-base*)
                               (output-start output-seq)
                               (digits 0))
  (if (>=-unsigned n base)
      (itoa-unsigned-loop n
                          output-seq
                          base
                          (expt-unsigned base (or digits (count-digits n base)))
                          output-start)
      (itoa-unsigned-finish n base output-seq output-start)))

(defun itoa-unsigned (n output-seq
                      &optional
                        (base *output-base*)
                        (padding *OUTPUT-LEADING-ZEROS*)
                        (output-start output-seq))
  (let ((digits (count-digits n base)))
    (itoa-unsigned-digits n
                          (if (and padding (>-unsigned padding digits))
                              (ptr-set output-seq (- padding digits 1) *OUTPUT-ZERO*)
                              output-seq)
                          base
                          output-start
                          digits)))

(defun itoa (n output-seq
             &optional
               (base *output-base*)
               (padding *OUTPUT-LEADING-ZEROS*))
  (if (< n 0)
      (itoa-unsigned (- n) (ptr-write-char *OUTPUT-NEGATIVE-SIGN* output-seq) base padding output-seq)
      (itoa-unsigned n output-seq base padding)))
