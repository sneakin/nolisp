;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/node_console")
(require "runtime/halt")

(var *banner* "WELCOME!!")

(node-console-write 'hello-world)
(node-console-write "\n\n")
(node-console-write *banner*)
(node-console-write "\n\nDec:\t")
(with-allocation (str 32)
  (node-console-write (itoa 1234 str))
  (node-console-write "\nHex:\t")
  (node-console-write (itoa #x-1234 str 16))
  (node-console-write "\n"))
(node-console-write "\n\nGood bye...\nDec:\t")

(halt)
