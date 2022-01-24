(in-package :nolisp)

(defun question-sym? (sym)
  (if (symbolp sym)
      (eq 0 (position #\? (symbol-name sym)))))

(defun match-var? (sym &optional (allow-keywords t))
  (or (question-sym? sym)
      (and allow-keywords (keywordp sym))))

(defun match-atom (pattern atom &key
			   syms
			   (allow-keywords t)
			   (varp (partial-after #'match-var? allow-keywords))
			   (test #'equal))
  (cond
    ((funcall varp pattern)
     (let ((old-value (assoc pattern syms)))
       (if old-value
           (if (funcall test atom (cdr old-value))
               (values syms T)
               (values syms nil))
           (values (acons pattern atom syms) T))))
    ((funcall test pattern atom) (values syms T))
    (t (values syms nil))))

(defun match (pattern lst &key
		      syms
		      (allow-keywords t)
		      (varp (partial-after #'match-var? allow-keywords))
		      (test #'equal))
  (cond
    ((or (atom pattern) (atom lst))
     (multiple-value-bind (syms matched)
         (match-atom pattern lst
                     :syms syms
                     :allow-keywords allow-keywords
		     :varp varp
                     :test test)
       (if matched (values syms t))))
    (t (multiple-value-bind (syms matched)
           (match (first pattern) (first lst)
                  :syms syms
                  :allow-keywords allow-keywords
		  :varp varp
                  :test test)
         (if matched
             (match (rest pattern) (rest lst)
                    :syms syms
                    :allow-keywords allow-keywords
		    :varp varp
                    :test test))))))
