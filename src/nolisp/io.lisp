(in-package :nolisp)

(defun read-exprs (io &optional acc)
  (let ((ex (read io nil)))
    (if ex
	(read-exprs io (cons ex acc))
	(nreverse acc))))

(defun each-read-expr (fn io &optional acc)
  (let ((ex (read io nil)))
    (if ex
	(each-read-expr fn io (multiple-value-list (apply fn (cons ex acc))))
	acc)))

(defun io-reader (io)
  #'(lambda () (read io nil)))
