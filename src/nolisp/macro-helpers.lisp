(in-package :nolisp)

(defun maybe-wrap-with-progn (forms)
  (if (< 1 (length forms))
      `(progn ,@forms)
    (first forms)))
