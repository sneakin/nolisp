(def f (x)
  (if x 1 0))

(values (f 123) (f 0))
(asm (halt))
