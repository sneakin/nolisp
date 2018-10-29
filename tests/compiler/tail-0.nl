(require "runtime/math")
(require "runtime/cmp")
(require "assert")

(var *loops* 20)

(defun stack-pointer ()
  (asm (mov 0 11)))

(defun g (&optional (sp (stack-pointer)) (n 0))
  (if (> n *LOOPS*)
    (stack-pointer)
    (g sp (+ n 1))))

(assert-equal (g (stack-pointer)) (+ (stack-pointer) 4))
(values (g) (stack-pointer))