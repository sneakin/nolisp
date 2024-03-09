(defsystem "nolisp2-compiler"
  :description "nolisp2-compiler: Nolan's Single Pass Lisp compiler binary"
  :version "2.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  :depends-on ("nolisp2")
  ;; :pathname "src/nolisp"
  :build-operation program-op
  :build-pathname "nolisp-compiler"
  :entry-point "repl-compiler-toplevel"
  :components (
    (:file "cli/arguments")
    (:file "cli/compiler" :depends-on ("cli/arguments"))
))
