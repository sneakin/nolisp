#!/usr/bin/env -S ecl --shell

(eval-when (:execute)
  (require :asdf)
  (require :nolisp-cli))

(in-package :nolisp-cli)

(eval-when (:execute)
  (start))
