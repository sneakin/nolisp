(defmacro nassert (a &optional msg (stream *error-output*))
  "An assert function for unit testing that when the asserted value is not nil prints '.' and when nil prints a supplied message."
  `(if ,a
       (progn (format ,(or stream *error-output*) ".")
	      nil)
     (progn (format ,(or stream *error-output*)
		    "~&Assertion failed: ~A~%~S~%~S~%" ,msg ',a ,a)
            t)))

(defmacro assert-with (fn a b &optional msg stream)
  "Asserts ~a~ returns ~b~ using ~fn~ as the truth function."
  `(let ((ar ,a))
     (nassert (funcall ,fn ar ,b)
	      ,(if msg msg `(format nil "~S =>~%~S~%not ~S~%~S" ',a ar ',fn ',b))
	      ,stream)))

(defmacro assert-eq (a b &optional msg stream)
  "Asserts ~a~ and ~b~ are eq."
  `(assert-with #'eq ,a ,b ,msg ,stream))

(defmacro assert-not-eq (a b &optional msg stream)
  "Asserts ~a~ and ~b~ are not eq."
  `(assert-with (compose #'eq #'not) ,a ,b ,msg ,stream))

(defmacro assert-equal (a b &optional msg stream)
  "Asserts ~a~ and ~b~ are equal."
  `(assert-with #'equal ,a ,b ,msg ,stream))

(defmacro assert-not-equal (a b &optional msg stream)
  "Asserts ~a~ and ~b~ are not equal."
  `(assert-with (compose #'equal #'not) ,a ,b ,msg ,stream))

(defun assert-cases (cases fn &optional stream)
  "Maps ~fn~ over a list of input and output pairs asserting ~fn~ passed the input returns the output."
  (mapcar #'(lambda (c)
	      (let ((result (funcall fn (first c))))
                (assert-equal result
                              (second c)
                              (format nil "~A~%~S => ~S~%Not ~S" (third c) (first c) result (second c))
			      stream)))
          cases))

(defmacro assert-values-equal (expr out-values &optional msg stream)
  "Asserts ~expr~ returns multiple values equal to the ~out-values~."
  `(assert-equal (multiple-value-list ,expr) ,out-values ,msg ,stream))

(defmacro assert-type-of (expr type &optional msg stream)
  "Asserts ~expr~ returns a value that is a certain type."
  `(assert-equal (type-of ,expr) ,type ,msg ,stream))

(defmacro assert-not-type-of (expr type &optional msg stream)
  "Asserts ~expr~ returns a value that is not ajcertain type."
  `(assert-not-equal (type-of ,expr) ,type ,msg ,stream))

(defmacro assert-throws (expr condition &optional msg stream)
  "Asserts ~expr~ raises a condition of certain type."
  `(restart-case
    (handler-bind ((condition #'(lambda (e)
				 (invoke-restart 'return-value (assert-type-of e ,condition ,msg ,stream)))))
      ,expr
      (format (or ,stream *error-output*) "Assertion failed: ~A error not raised.~%" ,condition)
      t)
    (return-value (v) v)))

(defmacro assert-does-not-throw (expr condition &optional msg stream)
  "Asserts ~expr~ does not raise a condition of a certain type."
  `(restart-case
    (handler-bind ((condition
		    #'(lambda (e)
			(invoke-restart 'return-value
					(assert-not-type-of e ,condition ,msg ,stream)))))
      ,expr
      nil)
    (return-value (v) v)))

(defmacro assert-contains (subseq expr &optional msg stream)
  "Asserts ~expr~ returns a sequence containing ~subseq~."
  `(assert-not-eq nil (search ,subseq ,expr) ,msg ,stream))

(defmacro assert-output (var contains expr &optional msg stream)
  "Asserts ~expr~ writes ~contains~ to a stream named by ~var~."
  `(assert-contains ,contains (with-output-to-string (,var) ,expr) ,msg ,stream))
