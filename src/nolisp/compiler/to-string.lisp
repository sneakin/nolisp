(in-package :nolisp)

(defun to-string-keyword? (sym)
  (if sym (not (not (position sym '(:newline :var :call :nonexit))))))

(defun to-string/2 (stream form &optional (was-key t))
  (when form
    (let ((arg (first form)))
      (cond
       ((eq arg :newline)
	(princ #\newline stream)
	(to-string/2 stream (rest form) t))
       ((to-string-keyword? arg)
	(to-string/2 stream (rest form) was-key))
       (t
	(unless was-key (princ #\space stream))
	(princ arg stream)
	(to-string/2 stream (rest form) nil))))))

(defun to-string (form &optional _state)
  (declare (ignore _state))
  (with-output-to-string (str)
    (to-string/2 str form)))
