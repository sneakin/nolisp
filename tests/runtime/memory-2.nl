(require "runtime/math")
(require "runtime/memory")

;(let ((a #xFFFE))
(with-allocation (a 16)
  (ptr-write-long #x12345687 a)
  (ptr-write-long #x12345678 (+ a 4))
  (values
    #xABCD
    (ptr-read-byte a)
    (ptr-read-byte (+ a 4))
    #x7890))
;)