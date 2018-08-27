(set f (lambda (x) x))

(set ptr-read (lambda (ptr offset)
                (asm (mov 0 1)
                     (cls)
                     (addi 2 14)
                     ;;(inc 0) 4
                     (load 0 0 0) 0)))

(set ptr-write (lambda (ptr offset value)
                 (asm (mov 0 1)
                      (cls)
                      (addi 2 14)
                      ;;(inc 0) 4
                      (store 3 0 0) 0)))

(with-allocation (a 64)
                 (ptr-write a 60 456)
                 (ptr-write a 0 123)
                 ;; zero registers
                 (values 0 0 0 0 0 0 0)
                 (values (ptr-read a 0)
                         ;; gets the wrong address for A
                         (ptr-read a 60)))

(asm (halt))
