(require "runtime/math.nl")
(require "runtime/string.nl")
;(values (string-equal "Hello" "Hello"))
(let ((str "Hello"))
  (values (ptr-read-long str) (ptr-read-byte str) (ptr-read-byte (+ str 1)))
  (asm (halt)))
