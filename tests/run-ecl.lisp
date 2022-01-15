#!/usr/bin/env -S ecl --shell
(load (make-pathname :name "run" :type "lisp" :directory (pathname-directory *load-truename*)))
(run-tests)
