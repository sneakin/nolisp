;;; -*- mode: Lisp; coding: utf-8-unix -*-

(var a 0)
(var b 0)

(var g (lambda (x)
         (set a x)))

(var h (lambda (x)
         (set b x)))

(var f (lambda (x y)
         (let ((z 3))
           (g z)
           (if y
               (h x)
             (h y)))))

(f 123 456)
(values a b)
(asm (halt))

