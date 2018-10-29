;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/node_console")
(require "runtime/halt")

(node-console-flush (node-console-write "Hello world\n"))
