(set nil 0)
(set t 't)

(set eq (lambda (a b)
          (asm (cmp 1 2)
               (load 0 0 15)
               0
               (load 0 1 15)
               1)))

(set f (lambda (x)
         (cond
          ((eq x 1) 100)
          ((eq x 2) 200)
          ((eq x 3) 300)
          (t 2))))

(values (f 1) (f 2) (f 3) (f 0))
(asm (halt))