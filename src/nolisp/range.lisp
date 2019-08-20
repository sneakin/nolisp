(require "nolisp/math")

(defun frange-inner (a b &optional (step 1) (rev t) (n 0) (fstep (/  step (- b a))) acc)
  (if (and (>= n 0) (< n 1.0))
      (frange-inner a b step rev (+ n fstep) fstep (cons (lerp a b n) acc))
      (if rev
          (nreverse acc)
          acc)))

(defun frange (a &optional b (step (if (and b (> a b)) -1 1)))
  (if b
      (frange-inner a b step)
      (if (< a 0)
          (frange-inner a 0 step)
          (frange-inner 0 a step))))

(defun range-inner (a b &optional (step 1) (n a) (rev t) acc)
  (let ((p (/ (- n a) (- b a))))
    (if (and (>= p 0) (< p 1))
        (range-inner a b step (+ n step) rev (cons n acc))
        (if rev
            (nreverse acc)
            acc))))

(defun range (a &optional b (step (if (and b (> a b)) -1 1)))
  (if b
      (range-inner a b step)
      (if (< a 0)
          (range-inner a 0 step)
          (range-inner 0 a step))))
