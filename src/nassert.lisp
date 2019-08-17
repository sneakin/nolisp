(defmacro nassert (a &optional msg (stream *error-output*))
  `(if ,a
       (progn (format ,stream ".")
              nil)
       (progn (format ,stream "~&Assertion failed: ~A~%~A~%" ,msg ,a)
              t)))

(defun assert-equal (a b &optional msg)
  (nassert (equal a b) (if msg msg (format nil "(equal ~A ~A)" a b))))

(defun assert-cases (cases fn)
  (mapcar #'(lambda (c) (let ((result (funcall fn (first c))))
                          (assert-equal result
                                        (second c)
                                        (format nil "~A~%'~A =>~%     ~A~%Not '~A" (third c) (first c) result (second c)))))
          cases))
