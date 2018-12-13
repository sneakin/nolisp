(in-package :repl-windows)

(require "cpu/x86")

(defun msdos-stub (out-ptr)
  (repl-amd64::emit-ops out-ptr
                        (push :cs)
                        (pop :ds)
                        ;; print message
                        (mov :dx #xE)
                        (mov :ah 9)
                        (int #x21)
                        ;; exit
                        (mov :ax #x4C01)
                        (int #x21)
                        "Hey
You!$"))
