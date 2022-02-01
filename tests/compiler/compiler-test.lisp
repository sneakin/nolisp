(defun test-compile-form ()
  (assert-matches '((ABC (ABC) "global symbols pass through")
                    (123 (123) "integers pass through")
                    ("Hello" ("Hello") "strings pass through")
                    ((+ 2 3 4) (4 3 2 + :nonexit) "math function call")
                    ((f 2 3 4) (4 3 2 f :nonexit) "named function call")
		    ((> x y) (x y > :nonexit) "reverses args to >")
		    ((>= x y) (x y >= :nonexit) "reverses args to >=")
		    ((< x y) (x y < :nonexit) "reverses args to <")
		    ((<= x y) (x y <= :nonexit) "reverses args to <=")
		    ((- x y) (x y - :nonexit) "reverses args to <=")
		    ((/ x y) (x y / :nonexit) "reverses args to /")
                    ;;((f (g)) (g f) "simple nested function call")
                    ((f (g))
		     (g :newline
			inner-frame "(" ?R ")" :newline
			0 argn f exit-frame :newline
			end-frame)
		     "simple nested function call")
                    ;;((f (g 2)) (2 g f exit-frame) "simple nested function call with arg")
                    ((f (g 2))
		     (2 g :newline
			inner-frame "(" ?R ")" :newline
			0 argn f exit-frame :newline
			end-frame)
		     "simple nested function call with arg")
                    ;;((f 1 (g 2) 3) (2 g 3 1 overn 1 f exit-frame) "simple nested function call surrounded by args")
                    ((f 1 (g 2) 3)
		     (2 g :newline
			inner-frame "(" ?R ")" :newline
			3 0 argn 1 f exit-frame :newline
			end-frame)
		     "simple nested function call surrounded by args")
                    ;;((+ 2 (* 3 4) 5) (4 3 * 5 1 OVERN 2 +) "nested function call")
                    ((+ 2 (* 3 4) 5)
		     (4 3 * :newline
			inner-frame "(" ?R ")" :newline
			5 0 argn 2 + exit-frame :newline
			end-frame)
		     "nested function call")
                    ;;((+ (* x x) (* y y)) (x x * y y * +) "Two nested function call")
                    ((+ (* x x) (* y y))
		     (x x * :newline
			inner-frame "(" ?R0 ")" :newline
			y y * :newline
			inner-frame "(" ?R1 ")" :newline
			0 argn 0 1 lookup + exit-frame :newline
			end-frame :newline
			end-frame)
		     "Two nested function call")
                    ((cons :key (list a (cons x y) (list b c (list d e f) g)))
                     ;; (
                     ;;  y x cons
		     ;;  f e d list
                     ;;  g 1 overn c b list
                     ;;  10 overn 1 overn a list
                     ;;  :key cons
		     ;; )
		     (y x cons :newline
			inner-frame "(" ?R0 ")" :newline
			f e d list :newline
			inner-frame "(" ?R1 ")" :newline
			g 0 argn c b list :newline
			inner-frame "(" ?R2 ")" :newline
			0 argn 0 2 lookup a list :newline
			inner-frame "(" ?R3 ")" :newline
			0 argn :key cons exit-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame)
		     "triple nested calls"
		     )
                    ((f (g 1 (list 1 2 3 4) (allot 16)) (list 16 32 64 128) 100)
                     (4 3 2 1 list :newline
			inner-frame "(" ?R0 ")" :newline
			16 allot :newline
			inner-frame "(" ?R1 ")" :newline
			0 argn 0 1 lookup 1 g :newline
			inner-frame "(" ?R2 ")" :newline
			128 64 32 16 list :newline
			inner-frame "(" ?R3 ")" :newline
			100 0 argn 0 1 lookup f exit-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame)
                     "can find arguments after an arbitray stack size increase")
                    ((/ 1 (/ 1 (/ 1 e)))
                     ;;(e 1 / 1 / 1 /)
		     (1 e / :newline
			inner-frame "(" ?R0 ")" :newline
			1 0 argn / :newline
			inner-frame "(" ?R1 ")" :newline
			1 0 argn / exit-frame :newline
			end-frame :newline
			end-frame)
		     "doubly nested, division operands ordered")
		    ((progn 1 (* 2 3) 4)
		     (3 2 * :newline
			inner-frame "(" ?R ")" :newline
			1 0 argn 4 exit-frame :newline
			end-frame)
		     "progn executes each arg, returns the last")
		    ((if x (+ 2 3))
		     (x IF :newline
			3 2 + :nonexit :newline
			ELSE :newline
			nil :newline
			THEN)
		     "IF w/ no else")
		    ((if x (+ 2 3) nil)
		     (x IF :newline
			3 2 + :nonexit :newline
			ELSE :newline
			nil :newline
			THEN)
		     "IF w/ else returning nil")
		    ((if x (+ 2 (+ 3 4)) nil)
		     (x IF :newline
			4 3 + :newline
			INNER-FRAME "(" ?R0 ")" :newline
			0 ARGN 2 + exit-frame :newline
			end-frame :newline
			ELSE :newline
			nil :newline
			THEN)
		     "IF w/ nested function call and else returning nil")
		    ((if x nil (+ 2 3))
		     (x IF :newline
			nil :newline
			ELSE :newline
			3 2 + :nonexit :newline
			THEN)
		     "IF w/ true clause returning nil")
		    ((if x nil (+ 2 (+ 3 4)))
		     (x IF :newline
			nil :newline
			ELSE :newline
			4 3 + :newline
			INNER-FRAME "(" ?R0 ")" :newline
			0 ARGN 2 + exit-frame :newline
			end-frame :newline
			THEN)
		     "IF w/ true clause returning nil and a nested call in the ELSE")
		    ((if (> x y) x y)
		     ;;(x y > IF x ELSE y THEN)
		     (x y > :newline
			inner-frame "(" TEST ")" :newline
			0 argn IF :newline
			x exit-frame :newline
			ELSE :newline
			y exit-frame :newline
			THEN :newline
			end-frame)
		     "top level if")
		    ((f (if (> x y) x y))
		     ;;(x y > IF x f ELSE y f THEN)
		     ("[" begin-frame "(" ?R ?CC ")" :newline
		      1 argn f :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?CL ")" :newline
		      x y > :newline
		      inner-frame "(" TEST ")" :newline
		      0 argn IF :newline
		      x 0 1 lookup exec exit-frame :newline
		      ELSE :newline
		      y 0 1 lookup exec exit-frame :newline
		      THEN :newline
		      end-frame :newline
		      end-frame)
		     "if as an arg")
                    ((defun fn () 123)
                     (":" fn "(" ")" :newline
                      begin-frame :newline
                      123
                      exit-frame :newline
		      end-frame :newline
                      ";")
                     "defun returning a value")
                    ((defun fn (x y) (+ x y))
                     (":" fn "(" y x ")" :newline
                      begin-frame :newline
                      1 argn 0 argn +
                      exit-frame :newline
		      end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun mag (x y) (+ (* x x) (* y y)))
                     (":" mag "(" y x ")" :newline
                      begin-frame :newline
                      0 argn 0 argn * :newline
		      inner-frame "(" ?R0 ")" :newline
                      1 1 lookup 1 1 lookup * :newline
		      inner-frame "(" ?R1 ")" :newline
                      0 argn 0 1 lookup + exit-frame :newline
		      end-frame :newline
		      end-frame :newline
		      end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun fn (x) (- 2 (if (> x 3) 3 4)))
                     (":" FN "(" X ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
		      "[" BEGIN-FRAME "(" ?R ?CC ")" :NEWLINE
		      2 1 ARGN - :NONEXIT END-FRAME :NEWLINE
		      "]" CLOSE-LAMBDA :NEWLINE
		      INNER-FRAME "(" ?CL ")" :NEWLINE
                      0 1 LOOKUP 3 > :NEWLINE
                      INNER-FRAME "(" TEST ")" :NEWLINE
                      0 ARGN IF :NEWLINE
                      3 0 1 LOOKUP EXEC EXIT-FRAME :NEWLINE
		      ELSE :NEWLINE
                      4 0 1 LOOKUP EXEC EXIT-FRAME :NEWLINE
		      THEN :NEWLINE
                      END-FRAME :NEWLINE
		      END-FRAME :NEWLINE
		      END-FRAME :NEWLINE
                      ";")
                     "subtraction and comparisons get swapped")
                    ((defun squarer () (lambda (x) (* x x)))
                     (":" squarer "(" ")" :newline
                      begin-frame :newline
                      "[" begin-frame "(" x ?cc ")" :newline
                      1 argn 1 argn *
                      :nonexit end-frame :newline
                      "]" close-lambda exit-frame :newline
		      end-frame :newline
                      ";")
                     "function returning an anonymous function")
		    ((defun f (x) (lambda (a) (+ x y a)))
		     (":" f "(" x ")" :newline
		      begin-frame :newline
		      "[" begin-frame "(" a ?cc ")" :newline
		      1 argn y 0 0 closure-lookup + :nonexit end-frame :newline
		      "]" close-lambda exit-frame :newline
		      end-frame :newline
		      ";")
		     "lambda with variable from caller")
		    ((defun f (a) (+ 2 a (if (> a 0) a (negate a))))
		     (":" f "(" a ")" :newline
		      begin-frame :newline
		      "[" begin-frame "(" ?R ?CC ")" :newline
		      1 argn 0 0 closure-lookup 2 + :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?CL ")" :newline
		      0 1 lookup 0 > :newline
		      inner-frame "(" TEST ")" :newline
		      0 argn IF :newline
		      0 2 lookup 0 1 lookup exec exit-frame :newline
		      ELSE :newline
		      0 2 lookup negate 0 1 lookup exec exit-frame :newline
		      THEN :newline
		      end-frame :newline
		      end-frame :newline
		      end-frame :newline
		      ";")
		     "if with variables from a function")
		    ((defun f (a) (+ 2 a (if (> a 0) (* a 2) (* a -2))))
		     (":" f "(" a ")" :newline
		      begin-frame :newline
		      "[" begin-frame "(" ?R ?CC ")" :newline
		      1 argn 0 0 closure-lookup 2 + :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?CL ")" :newline
		      0 1 lookup 0 > :newline
		      inner-frame "(" TEST ")" :newline
		      0 argn IF :newline
		      2 0 2 lookup * 0 1 lookup exec exit-frame :newline
		      ELSE :newline
		      -2 0 2 lookup * 0 1 lookup exec exit-frame :newline
		      THEN :newline
		      end-frame :newline
		      end-frame :newline
		      end-frame :newline
		      ";")
		     "if with variables as argumonts from a function")
		    ((mapcar (lambda (i) (* i i)) lst)
		     ("[" begin-frame "(" i ?cc ")" :newline
		      1 argn 1 argn * :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?R ")" :newline
		      lst 0 argn mapcar exit-frame :newline
		      end-frame)
		     "lambda as an argument")
		    (((lambda (x) (who x)) 3 4 5)
		     ("[" begin-frame "(" x ?cc ")" :newline
		      1 argn who :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?RL ")" :newline
		      5 4 3 0 argn exec exit-frame :newline
		      end-frame)
		     "lambda in first position")
		    ((let ((x (+ 2 3))
			   (y (* 2 2)))
		       (+ x y))
		     ("[" begin-frame "(" y x ?CC ")" :newline
		      2 argn 1 argn + :nonexit end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?RL ")" :newline
		      3 2 + :newline
		      inner-frame "(" ?RX ")" :newline
		      2 2 * :newline
		      inner-frame "(" ?RY ")" :newline
		      0 argn 0 1 lookup 0 2 lookup exec exit-frame :newline
		      end-frame :newline
		      end-frame :newline
		      end-frame)
		     "let forms"))
                  :fn (partial-after #'nolisp:compile-to-forth 'flatten)
                  :allow-keywords nil))

(defun test-compile-to-forth ()
  (let ((*gensym-counter* 1000))
    (assert-equal (nolisp:compile-to-forth '(defun f (x y) (+ (* x x) (* y y))))
		  ": F ( Y X )
BEGIN-FRAME
0 ARGN 0 ARGN *
INNER-FRAME ( R1000 )
1 1 LOOKUP 1 1 LOOKUP *
INNER-FRAME ( R1001 )
0 ARGN 0 1 LOOKUP + EXIT-FRAME
END-FRAME
END-FRAME
END-FRAME
;")))

(defun test-compile ()
  (test-compile-form)
  (test-to-string)
  (test-compile-to-forth)
  t)
