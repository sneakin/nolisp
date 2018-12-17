;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Convertors to integers

(require "runtime/cmp")
(require "runtime/math/float")
(require "runtime/convertors/float")
(require "runtime/number")

(defun float-to-int32 (f)
  (asm (load 0 0 11) 4
       (convf 0 8)))

(defun float-to-uint32 (f)
  (asm (load 0 0 11) 4
       (convf 0 0)))

(defun make-float-decimal (n f &optional (negate (< n 0)) (base *input-base*) (digits (count-digits n base)))
  (let ((out (+-float (float (abs-int n))
                      (if (> f 0)
                          (/-float (float f)
                                   (expt-float (float base)
                                               (float digits)))
                          0.0))))
    (if negate (- out) out)))
