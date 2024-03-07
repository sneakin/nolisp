NoLisp2
===

Copyright (C) 2018-2019 Nolan Eakins

A language that looks like Lisp but lacks lists and compiles in a single pass.
All it knows are integers and memory accesses.

https://github.com/sneakin/nolisp/tree/nolisp2

Requirements
---

* Steel Bank Common Lisp
* Ruby and Rake


Interactive Top Level
===

In a Lisp such as SBCL the following can be done:

Loading
---

```sh
   $ cd $ROOT
   $ sbcl
   cl> (load "loader.lisp")
```

Or in Emacs using Slime, evaluate `loader.lisp` with the `slime-eval-buffer` command.

Compiling Files
---

```lisp
   cl> (repl::repl-file path)
```

Disassembling Files
---

```lisp
   cl> (repl::disassemble-asm output-sequence)
```


Command Line
===

Compiling and disassembling can also be done outside of a Lisp:

Building
---

To produce `compiler.exe` and `disassembler.exe` run:

```sh
   $ cd $ROOT
   $ rake
```

Running
---

```sh
   $ ./compiler.exe -o hello-world.bin tests/runtime/hello-world.nl
```

And to run `hello-world.bin`:

```sh
   $ NODE_PATH=$BACAW/js/lib node $BACAW/bin/bccon.js hello-world.bin
```

Bacaw can be fetched using `git submodule update --init`.

