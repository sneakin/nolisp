NoLisp2
===

Copyright (C) 2018 Nolan Eakins

A language that looks like Lisp but lacks lists and compiles in a single pass.
All it knows are integers and memory accesses.


Interactive Top Level
===

Loading
---

   $ cd $ROOT/lisp
   $ sbcl
   > (load "sbcl.lisp")

Or in Emacs using Slime, evaluate `sbcl.lisp` with the `slime-eval-buffer` command.

Compiling Files
---

   > (repl::repl-file path)

Disassembling Files
---

   > (repl::disassemble-asm output-sequence)


Command Line
===

Building
---

To produce `compiler.exe` run:

   $ cd $ROOT/lisp
   $ ${SBCL} --script sbcl-image.lisp

Running
---

   $ ./compiler.exe -o hello-world.bin tests/runtime/hello-world.nl

And to run `hello-world.bin`:

   $ NODE_PATH=$ROOT node ${ROOT}/bccon.js hello-world.bin
