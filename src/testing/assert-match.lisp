(use-package :nolisp)

(defun assert-match (input pattern &optional msg (allow-keywords t))
     (multiple-value-bind (matches matched)
         (match pattern input :allow-keywords allow-keywords)
       (nassert matched
                (or msg (format nil "Pattern does not match:~%Input: ~S~%Pattern: ~S~%"
                                input pattern)))))

(defun assert-matches (cases &key (fn #'identity) msg (allow-keywords t))
  (mapcar #'(lambda (c)
              (let ((result (funcall fn (first c))))
                (assert-match result
                              (second c)
                              (format nil "Pattern does not match: ~A~%Input: ~S~%Pattern: ~S~%Result: ~S~%"
                                      (or (third c) msg "")
                                      (first c) (second c) result)
                              allow-keywords
                              )))
          cases))
