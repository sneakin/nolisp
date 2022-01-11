(defpackage :nolisp
  (:use :cl)
  (:export flatten clip-last nshift-left shift-left shift-right
	   ifeq napply compose partial-first partial-after
	   match
	   lerp
	   frange range
	   nc-scanner-error scan-list
	   nc-error-msg nc-error-form nc-error-state nc-error
	   nc-add-macro nc-remove-macro nc-macroexpand-1 nc-macroexpand
	   nc-lookup-resolver
	   nc-cps-transform
	   nc-list-compile nc-compile nc-to-string))
