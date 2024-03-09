(push (truename ".") asdf:*central-registry*)
(asdf:operate 'asdf:program-op :nolisp2-disasm)
(asdf:operate 'asdf:program-op :nolisp2-compiler)
(si:exit 0)