;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/node_console")
(require "runtime/halt")

(var *banner* "WELCOME!!")

(node-console-flush (node-console-write 'hello-world))
(node-console-flush (node-console-write "\n\n"))
(node-console-flush (node-console-write *banner*))
(node-console-flush (node-console-write "\n\nGood bye...\n"))

(halt)
