(defun f (x y)
  x)

(defun g (x)
  (let ((a #x123)
        (b #xFFFE)
        (c #xFFFF))
    (f x b))
    )

(g #x5678)
