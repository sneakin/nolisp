(defpackage :nolisp
  (:use :cl)
  (:export flatten clip-last nshift-left shift-left shift-right
	   ifeq compose partial-first partial-after
	   match
	   lerp
	   frange range
	   scanner-error scan-list
	   error-msg error-form error-state error
	   add-macro remove-macro macro-expand-1 macro-expand
	   lookup-resolver
	   cps-transform
	   compile-to-list compile-form to-string))
