(defun test-assert-contains ()
  (assert-eq nil (assert-contains "hey" "hey there!" "Asserts substring is found."))
  (with-output-to-string (s)
    (assert-eq t (assert-contains "boom" "hey there!" "Asserts substring is found and fails." s))))

(defun test-assert-output ()
  (assert-eq nil (assert-output s "Hey" (format s "You. Hey there.")
				"Asserts the string is in the output stream."))
  (assert-contains "Fails to find the string."
     (with-output-to-string (top-s)
       (assert-eq t (assert-output sf "Hey" (format sf "Not here.")
				   "Fails to find the string." top-s)))))

(defun test-assert-type-of ()
  (assert-eq nil (assert-type-of 'hello 'symbol "Checks the type."))
  (assert-output s "Assertion failed"
     (assert-eq t (assert-type-of 3 'symbol "Checks the type and fails." s)))
  (assert-eq nil (assert-not-type-of 'hello '(integer 3 3) "Checks it's not the type."))
  (assert-output s "Assertion failed"
    (assert-eq t (assert-not-type-of 'hello 'symbol "Checks it's not the type and fails." s))))

(defun test-assert-throws ()
  (assert-eq nil (assert-throws (error 'error) 'error "Asserts the error's type."))
  (assert-output s "Assertion failed"
    (assert-eq t (assert-throws (error 'error) 'simple-error
    				"Asserts the error's type and fails." s)))
  (assert-eq nil
	     (assert-does-not-throw (error 'error) 'simple-error
				    "Asserts when the error's type does not match."))
  (assert-output s "Assertion failed"
    (assert-eq t
    	       (assert-does-not-throw (error 'error) 'error
				      "Asserts when the error's type does not match and fails." s)))
  (assert-eq nil
  	     (assert-does-not-throw 3 'error
				    "Assertion passes with no error.")))

(defun test-assertions ()
  (test-assert-contains)
  (test-assert-output)
  (test-assert-type-of)
  (test-assert-throws)
  t)
