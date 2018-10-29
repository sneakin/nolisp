(require "runtime/eq")

(defun g (x)
  (eq x #x1234))

(defun h (a b)
  (values a b))
  
(defun f (y &optional b)
  (let ((x #x1234) (a #x4567))
    (if (eq x y)
        (g x)
        (g y))))

(defun do-it ()
  (with-allocation (a 20)
    (h (f 1) (f #x1234))))

(do-it)

