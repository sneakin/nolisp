;;; -*- mode: Lisp; coding: utf-8-unix -*-

(def a (x y z)
  (values x y z))
  
(def f (x &optional y)
  (values x y))

(def g (&optional x (y x))
  (values x y))

(def h1 ()
  (lambda fn (a b) (values a b)))
  
;; (def h2 ()
;;   (lambda fn (a &optional x (y x))
;;    (values x y)))
    
(values (f 1 2) (g) (h1))
