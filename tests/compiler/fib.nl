(set - (lambda (a b)
         (asm (mov 0 1)
              (cls)
              (subi 2))))

(set * (lambda (a b)
         (asm (mov 0 1)
              (muli 2))))

(set fact (lambda (n acc)
            (if n
                (fib (- n 1) (* n acc))
              0)))

(fact 5)
(asm (halt))
