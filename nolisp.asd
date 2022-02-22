(defsystem "nolisp"
  :description "nolisp: Nolan's Lisp"
  :version "3.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  ;; :depends-on ("optima.ppcre" "command-line-arguments")
  :pathname "src/nolisp"
  :components (
    (:file "package")
    (:file "list" :depends-on ("package"))
    (:file "fun" :depends-on ("package"))
    (:file "macro-helpers" :depends-on ("package"))
    (:file "assoc" :depends-on ("package"))
    (:file "match" :depends-on ("package" "assoc"))
    (:file "error" :depends-on ("package"))
    (:file "math" :depends-on ("package"))
    (:file "range" :depends-on ("package" "math"))
    (:file "scanner" :depends-on ("package" "error"))
    (:file "io" :depends-on ("package"))
    (:file "compiler/errors" :depends-on ("package" "error"))
    (:file "compiler/macroexpand" :depends-on ("package" "scanner"))
    (:file "macros/let" :depends-on ("package" "compiler/macroexpand"))
    (:file "macros/cond" :depends-on ("package" "macro-helpers" "compiler/macroexpand"))
    (:file "compiler/cps-transform" :depends-on ("package" "error" "scanner"))
    (:file "compiler/lookup-resolver" :depends-on ("package" "fun"))
    (:file "compiler/forthgen" :depends-on ("package" "scanner" "list" "fun"))
    (:file "compiler/to-string" :depends-on ("package"))
    (:file "compiler/toplevel" :depends-on ("package" "scanner" "list" "fun" "io" "compiler"))
    (:file "compiler"
	   :depends-on ("package"
			"compiler/errors"
			"compiler/macroexpand"
			"macros/let"
			"macros/cond"
			"compiler/cps-transform"
			"compiler/lookup-resolver"
			"compiler/forthgen"
			))))
