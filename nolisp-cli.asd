(defsystem "nolisp-cli"
  :description "nolisp-cli: Nolan's Lisp Command Line Interface"
  :version "3.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  :depends-on ("nolisp")
  :build-pathname "nolisp3"
  :entry-point "nolisp-cli:start"
  :pathname "src/nolisp-cli"
  :components ((:file "package")
               #+ecl(:file "main-ecl")
               #+sbcl(:file "main-sbcl")
	       (:file "main" :depends-on ("package" #+ecl"main-ecl" #+sbcl"main-sbcl"))))
