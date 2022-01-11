(defun test-nc-compile ()
  (assert-matches '((ABC (ABC) "global symbols pass through")
                    (123 (123) "integers pass through")
                    ("Hello" ("Hello") "strings pass through")
                    ((+ 2 3 4) (2 3 4 + return) "math function call")
                    ((f 2 3 4) (4 3 2 f return) "named function call")
                    ((f (g)) (g f return) "simple nested function call")
                    ((f (g 2)) (2 g f return) "simple nested function call with arg")
                    ((+ 2 (* 3 4) 5)
                     (4 3 *
                      5 1 OVERN
                      2 +)
                     "nested function call")
                    ((+ (* x x)
                      (* y y))
                     (x x *
                      y y *
                      4 overn +)
                     "Two nested function call")
                    ((cons :key (list a (cons x y) (list b c (list d e f) g)))
                     (f e d list
                      g 1 overn c b list
                      y x cons
                      c 1 overn a list
                      :key cons))
                    ((f (g 1 (list 1 2 3 4) (allot 16)) (progn 16 32 64 128) 100)
                     (4 3 2 1 list
                      16 allot
                      17 overn 1 g
                      16 32 64 128
                      100 1 overn 6 overn f)
                     "can find arguments after an arbitray stack size increase")
                    ((/ 1 (/ 1 (/ 1 e))) ; todo proper math arg ordering
                     (e 1 /
                      1 /
                      1 /)
                     "doubly nested")
                    ((defun fn ()
                       123)
                     (":" fn "(" ")" :NEWLINE
                      begin-frame :NEWLINE
                      123
                      frame-return :NEWLINE
                      ";")
                     "defun returning a value")
                    ((defun fn (x y)
                       (+ x y))
                     (":" fn "(" x y ")" :NEWLINE
                      begin-frame :NEWLINE
                      1 argn 0 argn +
                      frame-return :NEWLINE
                      ";")
                     "defun with one call")
                    ((defun mag (x y)
                       (+ (* x x) (* y y)))
                     (":" mag "(" x y ")" :NEWLINE
                      begin-frame :NEWLINE
                      0 argn 0 argn * :NEWLINE
                      1 argn 1 argn * :NEWLINE
                      2 overn 3 overn +
                      frame-return :NEWLINE
                      ";")
                     "defun with one call")
                    ((defun fn (x) (- 2 (if (> x 3) 3 4)))
                     (":" FN "(" CC X ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
                      ARGN> 1 3 > "(" TEST ")" :NEWLINE
                      TAIL-FRAME :NEWLINE
                      ARGN> 0 IF :NEWLINE
                      2 3 - LOOKUP> 1 0 ELSE :NEWLINE
                      2 4 - LOOKUP> 1 0 THEN :NEWLINE
                      FRAME-RETURN :NEWLINE
                      ";")
                     "subtraction and comparisons get swapped")
                    ((defun squarer () (lambda (x) (* x x)))
                     (":" squarer "(" ?ra ")" :newline
                      begin-frame :newline
                      "[" begin-frame "(" ?ra1 ?fp x ")":newline
                      0 argn
                      2 argn 2 argn *
                      frame-return :newline
                      "]" current-frame close-lambda :newline
                      frame-return :newline
                      ";")
                     "function returning an anonymous function"))
                  :fn #'nc-compile
                  :allow-keywords nil))
