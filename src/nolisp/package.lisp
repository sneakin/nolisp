(defpackage :nolisp
  (:use :cl)
  (:export :fix-improper-list :fix-improper-list! :improper-mapcar :flatten :clip-last :nshift-left :shift-left :shift-right :reduce-values
	   :ifeq :compose :partial-first :partial-after
	   :match
	   :lerp
	   :frange :range
	   :scanner-error :scan-list :10scan-list-reducer-fn :scan-list-reducer
	   :error-msg :error-form :error-state :error
	   :*macros* :macro? :add-macro :remove-macro :macro-expand-1 :macro-expand
	   :lookup-resolver
	   :cps-transform
	   :*stages* :compile-to-forth
	   :compile-to-lookup :compile-to-list :compile-to-flatlist :to-string :compile-to-string))
