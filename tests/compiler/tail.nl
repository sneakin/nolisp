(set a 0)
(set b 0)

(set g (lambda (x)
         (set a x)))

(set h (lambda (x)
         (set b x)))

(set f (lambda (x y)
         (let ((z 3))
           (g z)
           (if y
               (h x)
             (h y)))))

(f 123 456)
(values a b)
(asm (halt))

