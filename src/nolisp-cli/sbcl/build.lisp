#!/usr/bin/env -S sbcl --script

(require :asdf)
(require :nolisp-cli)

(defun build-bin ()
  (asdf:operate 'asdf:program-op :nolisp-cli)
  (sb-ext:run-program "/bin/cp" (list (sb-ext:native-namestring (first (asdf:output-files 'asdf:program-op :nolisp-cli))) "nolisp3")))

(eval-when (:execute) (build-bin))
