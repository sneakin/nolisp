(defun test-compile ()
  (assert-matches '((ABC (ABC exit-frame) "global symbols pass through")
                    (123 (123 exit-frame) "integers pass through")
                    ("Hello" ("Hello" exit-frame) "strings pass through")
                    ((+ 2 3 4) (2 3 4 + exit-frame) "math function call")
                    ((f 2 3 4) (4 3 2 f exit-frame) "named function call")
                    ((f (g)) (g f exit-frame) "simple nested function call")
                    ((f (g 2)) (2 g f exit-frame) "simple nested function call with arg")
                    ((f 1 (g 2) 3) (2 g 3 2 overn 1 f exit-frame) "simple nested function call surrounded by args")
                    ((+ 2 (* 3 4) 5) (4 3 * 5 1 OVERN 2 +) "nested function call")
                    ((+ (* x x) (* y y)) (x x * y y * +) "Two nested function call")
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
                     (e 1 / 1 / 1 /) "doubly nested")
		    ((if (> x y) x y) (y x > IF x ELSE y THEN) "top level if")
                    ((defun fn () 123)
                     (":" fn "(" ")" :newline
                      begin-frame :newline
                      123
                      frame-return :newline
		      end-frame :newldne
                      ";")
                     "defun returning a value")
                    ((defun fn (x y) (+ x y))
                     (":" fn "(" x y ")" :newline
                      begin-frame :newline
                      1 argn 0 argn +
                      frame-return :newline
		      end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun mag (x y) (+ (* x x) (* y y)))
                     (":" mag "(" x y ")" :newline
                      begin-frame :newline
                      0 argn 0 argn * :newline
                      1 argn 1 argn * :newline
                      2 overn 3 overn +
                      frame-return :newline
		      end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun fn (x) (- 2 (if (> x 3) 3 4)))
                     (":" FN "(" CC X ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
                      1 ARGN 3 > "(" TEST ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
                      0 ARGN IF :NEWLINE
                      2 3 - 0 1 LOOKUP ELSE :NEWLINE
                      2 4 - 0 1 LOOKUP THEN :NEWLINE
                      FRAME-RETURN :NEWLINE
		      END-FRAME :newline
		      END-FRAME :newline
                      ";")
                     "subtraction and comparisons get swapped")
                    ((defun squarer () (lambda (x) (* x x)))
                     (":" squarer "(" ")" :newline
                      begin-frame :newline
                      "[" begin-frame "(" x ")":newline
                      0 argn 0 argn *
                      frame-return :newline
		      end-frame :newline
                      "]" current-frame close-lambda :newline
                      frame-return :newline
		      end-frame :newline
                      ";")
                     "function returning an anonymous function"))
                  :fn #'nolisp:compile-form
                  :allow-keywords nil))

(defun test-to-string ()
  (assert-cases '(((hello world) "HELLO WORLD")
		  ((hello nil world nil) "HELLO NIL WORLD NIL")
		  ((hello :call world :var) "HELLO WORLD")
		  ((:var hello :call world) "HELLO WORLD")
		  ((hello :call world :var 123) "HELLO WORLD 123")
		  ((hello :newline world) "HELLO
WORLD")
		  ((":" squarer "(" x ")" :newline
				    begin-frame :newline
				    "[" begin-frame "(" x ")" :newline
				    0 argn 0 argn *
				    frame-return :newline
				    end-frame :newline
				    "]" current-frame close-lambda :newline
				    frame-return :newline
				    end-frame :newline
				    ";")
		": SQUARER ( X )
BEGIN-FRAME
[ BEGIN-FRAME ( X )
0 ARGN 0 ARGN * FRAME-RETURN
END-FRAME
] CURRENT-FRAME CLOSE-LAMBDA
FRAME-RETURN
END-FRAME
;"))
		#'nolisp:to-string))
