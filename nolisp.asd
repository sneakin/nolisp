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
    (:file "match" :depends-on ("package"))
    (:file "error" :depends-on ("package"))
    (:file "math" :depends-on ("package"))
    (:file "range" :depends-on ("package" "math"))
    (:file "scanner" :depends-on ("package" "error"))
    (:file "compiler/errors" :depends-on ("package" "error"))
    (:file "compiler/macroexpand" :depends-on ("package" "scanner"))
    (:file "compiler/cps-transform" :depends-on ("package" "error" "scanner"))
    (:file "compiler/lookup-resolver" :depends-on ("package" "fun"))
    (:file "compiler/forthgen" :depends-on ("package" "scanner" "list" "fun"))
    (:file "compiler"
	   :depends-on ("package"
			"compiler/errors"
			"compiler/macroexpand"
			"compiler/cps-transform"
			"compiler/lookup-resolver"
			"compiler/forthgen"))))