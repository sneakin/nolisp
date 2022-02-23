;#!/usr/bin/env -S sbcl --script

(eval-when (:execute)
  (require :asdf)
  (require :nolisp-cli))

(in-package :nolisp-cli)

(eval-when (:execute)
  (start))
