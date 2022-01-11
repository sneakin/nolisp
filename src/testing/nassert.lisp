(defmacro nassert (a &optional msg (stream *error-output*))
  `(if ,a
       (progn (format ,stream ".")
              nil)
       (progn (format ,stream "~&Assertion failed: ~S~%~S~%~S~%" ,msg ',a ,a)
              t)))

(defmacro assert-equal (a b &optional msg)
  `(let ((ar ,a))
     (nassert (equal ar ,b) ,(if msg msg `(format nil "~S => ~S not ~S" ',a ar ',b)))))

(defun assert-cases (cases fn)
  (mapcar #'(lambda (c)
	      (let ((result (funcall fn (first c))))
                (assert-equal result
                              (second c)
                              (format nil "~A~%'~A =>~%     ~A~%Not '~A" (third c) (first c) result (second c)))))
          cases))
