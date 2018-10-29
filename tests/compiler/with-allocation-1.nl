;;; Test with-allocation's tail call capability.
(defun f (a) a)

(defun g (a)
(with-allocation (x 12)
  (f x)))

(g #x1234)
(asm (halt))
;;; R0 = (+ SP 12)
