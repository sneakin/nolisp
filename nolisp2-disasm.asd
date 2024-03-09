(defsystem "nolisp2-disasm"
  :description "nolisp2-disasm: Nolan's Single Pass Lisp disassembler binary"
  :version "2.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  :depends-on ("nolisp2")
  ;; :pathname "cli"
  :build-operation program-op
  :build-pathname "nolisp-disasm"
  :entry-point "repl-disasm-toplevel"
  :components (
    (:file "cli/arguments")
    (:file "cli/disasm" :depends-on ("cli/arguments"))
))
