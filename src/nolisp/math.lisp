(in-package :nolisp)

(defun lerp (a b n)
  (+ (* a (- 1 n)) (* b n)))
