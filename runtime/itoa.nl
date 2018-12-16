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

;; fixme need constant access at compile time or variable with-allocations
;;(defconstant *itoa-max-length* 36)

(defun itoa-unsigned-finish (n base output-seq output-start)
  (values output-start (ptr-write-char 0 (ptr-write-char (char-digit n base) output-seq))))

(defun itoa-unsigned-loop (n output-seq &optional (base *output-base*) (divisor (max-digit-value n base)) (output-start output-seq))
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

(defun itoa-unsigned (n output-seq &optional (base *output-base*) (output-start output-seq))
  (if (>=-unsigned n base)
      (itoa-unsigned-loop n output-seq base (max-digit-value n base) output-seq)
      (itoa-unsigned-finish n base output-seq output-start)))

(defun itoa (n output-seq &optional (base *output-base*))
  (if (< n 0)
      (itoa-unsigned (- n) (ptr-write-char #\- output-seq) base output-seq)
      (itoa-unsigned n output-seq base output-seq)))
