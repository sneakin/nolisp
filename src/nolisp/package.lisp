(defpackage :nolisp
  (:use :cl)
  (:export :last? :improper-cell? :fix-improper-list :fix-improper-list! :improper-mapcar :mapl-improper :map-improper :mapelt-improper :flatten :nth-cons :nth-cons-from-end :clip-last :nshift-left :shift-left :shift-right :reduce-values
           :ifeq :ifequal :compose :partial-first :partial-after
           :assoc-get :assoc-get-all :assoc-bind
	   :match :when-match-bind :match-bind :match-bind! :match-error :match-case
	   :lerp
	   :frange :range
	   :scanner-error :scan-list :scan-list-reducer
	   :error-msg :error-form :error-state :error
	   :*macros* :macro? :add-macro :remove-macro :macro-expand-1 :macro-expand
	   :lookup-resolver
	   :cps-transform
	   :*stages* :compile-to-forth
	   :compile-to-lookup :compile-to-list :compile-to-flatlist :to-string :compile-to-string
           :toplevel-default-states :toplevel-compile :toplevel-compile-stream :toplevel-compile-file))
