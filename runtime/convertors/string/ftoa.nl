;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/convertors/string/globals")
(require "runtime/convertors/string/itoa")

(defun ftoa (n output-seq
             &optional
               (base *output-base*)
               (places (float *OUTPUT-PRECISION*))
               (padding *OUTPUT-LEADING-ZEROS*))
  (let* ((abs-n (abs-float n))
         (i (floorf abs-n))
         (d (*-float (expt-float (float base) places) (--float abs-n i))))
    (multiple-value-bind (start ending)
        (itoa-unsigned (float-to-uint32 i)
                       (if (<-float n 0.0)
                           (if *OUTPUT-NEGATIVE-SIGN* (ptr-write-char *OUTPUT-NEGATIVE-SIGN* output-seq) output-seq)
                           (if *OUTPUT-POSITIVE-SIGN* (ptr-write-char *OUTPUT-POSITIVE-SIGN* output-seq) output-seq))
                       base
                       padding
                       output-seq)
      (itoa-unsigned (float-to-uint32 d)
                     (ptr-write-char *OUTPUT-FRACTION-SIGN* (- ending 1))
                     base
                     (float-to-int32 places)
                     output-seq))))

