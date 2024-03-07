(load "loader.lisp")

(defparameter *compiler-sources*
  '("repl" "cli/compiler" "cli/arguments" "compiler"
    "globals" "type-sizes" "conditions" "memory"
    "sequence" "symbol" "string" "compiler/package" "reader"
    "features" "logging" "cpu/bacaw/emitter" "env"))
(defparameter *disasm-sources*
  '("repl" "cli/disasm" "cli/arguments" "disassembler" "cpu/bacaw/bacaw" "logging"))
  
(dolist (src (concatenate 'list *compiler-sources* *disasm-sources*))
  (compile-file (make-pathname :name src :type "lisp") :system-p t))
  
(c:build-program "test-comp.elf"
  :lisp-files (mapcar #'(lambda (p) (make-pathname :name p :type "o")) *compiler-sources*)
  ;;:epilogue-code '(repl-imager-toplevel (ext:command-args)))
  :epilogue-code '(repl-compiler-toplevel (ext:command-args)))

(c:build-program "test-disasm.elf"
  :lisp-files (mapcar #'(lambda (p) (make-pathname :name p :type "o")) *disasm-sources*)
  ;;:epilogue-code '(repl-imager-toplevel (ext:command-args)))
  :epilogue-code '(repl-disasm-toplevel (ext:command-args)))
  