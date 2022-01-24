(defun assert-anymatch (input pattern
			      &key
			      (modifier #'identity)
			      msg
			      (allow-keywords t)
			      (varp (partial-after #'nolisp::match-var? allow-keywords)))
  (multiple-value-bind (matches matched)
      (nolisp:match pattern input :allow-keywords allow-keywords :varp varp)
    (nassert (funcall modifier matched)
             (or msg (format nil "Pattern does not match:~%Input: ~S~%Pattern: ~S~%"
                             input pattern)))))

(defun assert-match (input pattern
			   &key
			   msg
			   (allow-keywords t)
			   (varp (partial-after #'nolisp::match-var? allow-keywords)))
  (assert-anymatch input pattern
		   :allow-keywords allow-keywords
		   :varp varp))

(defun assert-mismatch (input pattern
			      &key
			      msg
			      (allow-keywords t)
			      (varp (partial-after #'nolisp::match-var? allow-keywords)))
  (assert-anymatch input pattern
		   :modifier #'not
		   :msg (or msg (format nil "Pattern does match:~%Input: ~S~%Pattern: ~S~%"
					input pattern))
		   :allow-keywords allow-keywords
		   :varp varp))

(defun assert-matches (cases &key
			     (fn #'identity)
			     msg
			     (allow-keywords t)
			     (varp (partial-after #'nolisp::match-var? allow-keywords)))
  (mapcar #'(lambda (c)
              (let ((result (funcall fn (first c))))
                (assert-match result
                              (second c)
                              :msg (format nil "Pattern does not match: ~A~%Input: ~S~%Pattern: ~S~%Result: ~S~%"
					   (or (third c) msg "")
					   (first c) (second c) result)
                              :allow-keywords allow-keywords
			      :varp varp)))
          cases))

(defun assert-mismatches (cases &key
				(fn #'identity)
				msg
				(allow-keywords t)
				(varp (partial-after #'nolisp::match-var? allow-keywords)))
  (mapcar #'(lambda (c)
              (let ((result (funcall fn (first c))))
                (assert-mismatch result
				 (second c)
				 :msg (format nil "Pattern does match: ~A~%Input: ~S~%Pattern: ~S~%Result: ~S~%"
					      (or (third c) msg "")
					      (first c) (second c) result)
				 :allow-keywords allow-keywords
				 :varp varp)))
          cases))
