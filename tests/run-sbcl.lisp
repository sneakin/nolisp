#!/usr/bin/env -S sbcl --script
(load (make-pathname :name "run" :type "lisp" :directory (pathname-directory *load-truename*)))
(run-tests)
