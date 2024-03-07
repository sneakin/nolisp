#+:sbcl
(defun command-args () sb-ext:*posix-argv*)
#+:ecl
(defun command-args () (ext:command-args))

#+:sbcl
(defvar *shell-args* '("--script"))
#+:ecl
(defvar *shell-args* '("--shell"))

(defun remove-shell-args (args)
  (if args
      (if (find (car args) *shell-args* :test #'string=)
	  (cddr args)
	  (cons (car args) (remove-shell-args (rest args))))))

(defun parse-command-line-arg (args)
  (let ((arg (first args)))
    (cond
      ((or (string= arg "-help")
           (string= arg "--help")
           (string= arg "-h")) (values :help t (rest args)))
      ((or (string= arg "-o")
           (string= arg "--output"))  (values :output (second args) (rest (rest args))))
      ((or (string= arg "-L")
           (string= arg "--search"))  (values :search-path (second args) (rest (rest args))))
      ((string= arg "--log-level") (values :log-level (second args) (rest (rest args))))
      ((string= arg "--no-tail") (values :no-tail t (rest args)))
      (t (values nil nil (rest args) (first args))))))

(defun parse-command-line-args (args &optional options unused)
  (if args
      (multiple-value-bind (name value args unknown)
          (parse-command-line-arg args)
        (parse-command-line-args args
                                 (if name (cons (cons name value) options) options)
                                 (if unknown (cons unknown unused) unused)))
      (values options unused)))
