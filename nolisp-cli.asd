(defsystem "nolisp-cli"
  :description "nolisp-cli: Nolan's Lisp Command Line Interface"
  :version "3.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  :depends-on ("nolisp")
  :build-operation program-op
  :build-pathname "nolisp3"
  :entry-point "nolisp-cli:start"
  :pathname "src/nolisp-cli"
  :components ((:file "package")
	       (:file "options")
               #+ecl(:file "ecl/main")
               #+sbcl(:file "sbcl/main")
	       (:file "main" :depends-on ("package" "options"
					  #+ecl"ecl/main"
					  #+sbcl"sbcl/main"))))
