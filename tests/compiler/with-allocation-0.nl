;;; Test with-allocation's tail call capability.
(defun f (a) a)

(with-allocation (x 12)
  (f x))

;;; R0 = (+ SP 12)
