(require "runtime/math")
(require "runtime/memory")

(let ((a #xFFFE))
(with-allocation (a 12)
  (values (ptr-read-long a)
    a
    (ptr-write-long #x5678 (ptr-write-long #x1234 a))
    (ptr-read-long a)
    (ptr-read-long (+ a 4))
    (ptr-read-ulong a))))
