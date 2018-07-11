(in-package :repl)

#-:sbcl
(defun assert (v)
  (if (not v)
      (error 'assertion-failed)))
