(defmacro nassert (a &optional msg (stream *error-output*))
  `(if ,a
       (progn (format ,stream ".")
              nil)
       (progn (format ,stream "~&Assertion failed: ~S~%~S~%~S~%" ,msg ',a ,a)
              t)))

(defmacro assert-with (fn a b &optional msg)
  `(let ((ar ,a))
     (nassert (,fn ar ,b)
	      ,(if msg msg `(format nil "~S => ~S not ~S ~S" ',a ar ',fn ',b)))))

(defmacro assert-eq (a b &optional msg)
  `(assert-with eq ,a ,b ,msg))

(defmacro assert-equal (a b &optional msg)
  `(assert-with equal ,a ,b ,msg))

(defun assert-cases (cases fn)
  (mapcar #'(lambda (c)
	      (let ((result (funcall fn (first c))))
                (assert-equal result
                              (second c)
                              (format nil "~A~%'~A =>~%     ~A~%Not '~A" (third c) (first c) result (second c)))))
          cases))

(defmacro assert-values-equal (expr out-values &optional msg)
  `(assert-equal (multiple-value-list ,expr) ,out-values ,msg))
