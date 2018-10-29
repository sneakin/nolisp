(require "runtime/math")
(require "runtime/memory")

(let ((x #xFFFE))
(with-allocation (a 12)
  (ptr-write-long #x12345678 a)
  (values
    (ptr-read-long a)
    (ptr-read-byte a)
    (ptr-read-ubyte a)
    a
    (ptr-write-ubyte -64 (ptr-write-ubyte #x12 a))
    (ptr-read-ubyte a)
    (ptr-read-ubyte (+ a 1))))
)