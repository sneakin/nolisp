(defsystem "nolisp2"
  :description "nolisp2: Nolan's Single Pass Lisp"
  :version "2.0.1"
  :author "Nolan Eakins <sneakin@semanticgap.com>"
  :licence "Private"
  ;; :depends-on ("optima.ppcre" "command-line-arguments")
  ;; :pathname "src/nolisp"
  :components (
    (:file "repl")
    (:file "loader" :depends-on ("repl"))
    (:file "assert" :depends-on ("repl" "loader"))
    (:file "cli/arguments" :depends-on ("repl" "loader"))
    (:file "cli/compiler" :depends-on ("repl" "loader"))
    (:file "cli/disasm" :depends-on ("repl" "loader"))
    #+:ecl (:file "cli/ecl-builder" :depends-on ("repl" "loader"))
    #+:sbcl (:file "cli/sbcl-image" :depends-on ("repl" "loader"))
    (:file "runtime/defstruct" :depends-on ("repl" "type-sizes" "symbol"))
    (:file "runtime/bc/cpu" :depends-on ("repl"))
    (:file "compiler" :depends-on ("repl" "loader" "globals" "type-sizes"
    "conditions" "memory" "sequence" "symbol" "string" "compiler/package"
    "reader" "features" "logging" "cpu/bacaw/emitter" "env"))
    (:file "compiler/symbol-index" :depends-on ("repl" "loader" "runtime/defstruct"))
    (:file "compiler/byte-buffer" :depends-on ("repl" "loader" "type-sizes" "memory" "string" "runtime/defstruct"))
    (:file "compiler/imports-list" :depends-on ("repl" "loader" "runtime/defstruct" "type-sizes"))
    (:file "compiler/package" :depends-on ("repl" "loader" "runtime/defstruct" "type-sizes" "compiler/byte-buffer" "compiler/symbol-index" "compiler/imports-list"))
    (:file "conditions" :depends-on ("repl" "loader"))
    (:file "cpu" :depends-on ("repl" "loader" "runtime/bc/cpu"))
    (:file "cpu/bacaw/bacaw" :depends-on ("repl" "loader" "cpu/bacaw/isa"))
    (:file "cpu/bacaw/emitter" :depends-on ("repl" "loader" "memory"
      "symbol" "string" "type-sizes" "logging" "runtime/bc/cpu"))
    (:file "cpu/bacaw/isa" :depends-on ("repl" "loader" "conditions" "type-sizes"))
    (:file "defenum" :depends-on ("repl" "loader" "symbol"))
    (:file "disassembler" :depends-on ("repl" "loader" "cpu/bacaw/bacaw"))
    (:file "env" :depends-on ("repl" "loader" "memory" "type-sizes" "symbol"
      "logging"))
    (:file "features" :depends-on ("repl" "loader"))
    (:file "file-outputter" :depends-on ("repl" "loader" "memory" "compiler"
      "outputter" "logging"))
    (:file "globals" :depends-on ("repl" "loader"))
    (:file "logging" :depends-on ("repl" "loader" "defenum" ))
    (:file "memory" :depends-on ("repl" "loader" "type-sizes"))
    (:file "outputter" :depends-on ("repl" "loader" "logging" "memory" "symbol" "cpu/bacaw/emitter" "compiler/package" "runtime/bc/cpu"))
    (:file "reader" :depends-on ("repl" "loader" "conditions" "memory"
      "symbol" "sequence" "logging"))
    (:file "sequence" :depends-on ("repl" "loader" "memory"))
    (:file "string" :depends-on ("repl" "loader"))
    (:file "symbol-gen" :depends-on ("repl" "loader" "symbol" "string"))
    (:file "symbol" :depends-on ("repl" "loader" "globals" "memory"))
    (:file "test" :depends-on ("repl" "loader" "compiler" "outputter"
      "file-outputter" "assert"))
    (:file "token-seq" :depends-on ("repl" "loader"))
    (:file "type-sizes" :depends-on ("repl" "loader" "cpu"))
))