(in-package :nolisp-cli)

(defconstant *default-option-flags* '("help" "debug"))

(defun is-option? (str)
  (eq #\- (elt str 0)))
(defun option-name (str)
  (if (is-option? str)
      (subseq str (if (eq #\- (elt str 1)) 2 1))))

(defun parse-options (args
		      &key
			(flags *default-option-flags*)
			collections
			values
			left-over)
  (cond
    ((eq args nil) (values values left-over))
    ((find (option-name (first args)) flags :test #'equal)
     (parse-options (rest args)
		    :flags flags
		    :values (acons (option-name (first args)) t values)
		    :left-over left-over))
    ((find (option-name (first args)) collections :test #'equal)
     (parse-options (rest (rest args))
		    :flags flags
		    :values (acons (option-name (first args)) (second args) values)
		    :left-over left-over))
    ((is-option? (first args))
     (parse-options (rest (rest args))
		    :flags flags
		    :values (acons (option-name (first args)) (second args) values)
		    :left-over left-over))
    (t (parse-options (rest args)
		      :flags flags
		      :values values
		      :left-over (cons (first args) left-over)))))

(defun option-value (option alist)
  (nolisp:assoc-get option alist :test #'equal))
