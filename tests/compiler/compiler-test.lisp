(defun test-compile-form ()
  (assert-matches '((ABC (ABC exit-frame) "global symbols pass through")
                    (123 (123 exit-frame) "integers pass through")
                    ("Hello" ("Hello" exit-frame) "strings pass through")
                    ((+ 2 3 4) (4 3 2 + exit-frame) "math function call")
                    ((f 2 3 4) (4 3 2 f exit-frame) "named function call")
                    ;;((f (g)) (g f exit-frame) "simple nested function call")
                    ((f (g))
		     (g :newline
			inner-frame "(" ?R ")" :newline
			0 argn f exit-frame end-frame :newline)
		     "simple nested function call")
                    ;;((f (g 2)) (2 g f exit-frame) "simple nested function call with arg")
                    ((f (g 2))
		     (2 g :newline
			inner-frame "(" ?R ")" :newline
			0 argn f exit-frame end-frame :newline)
		     "simple nested function call with arg")
                    ;;((f 1 (g 2) 3) (2 g 3 1 overn 1 f exit-frame) "simple nested function call surrounded by args")
                    ((f 1 (g 2) 3)
		     (2 g :newline
			inner-frame "(" ?R ")" :newline
			3 0 argn 1 f exit-frame end-frame :newline)
		     "simple nested function call surrounded by args")
                    ;;((+ 2 (* 3 4) 5) (4 3 * 5 1 OVERN 2 +) "nested function call")
                    ((+ 2 (* 3 4) 5)
		     (4 3 * :newline
			inner-frame "(" ?R ")" :newline
			5 0 argn 2 + exit-frame end-frame :newline)
		     "nested function call")
                    ;;((+ (* x x) (* y y)) (x x * y y * +) "Two nested function call")
                    ((+ (* x x) (* y y))
		     (x x * :newline
			inner-frame "(" ?R0 ")" :newline
			y y * :newline
			inner-frame "(" ?R1 ")" :newline
			0 argn 0 1 lookup + exit-frame end-frame :newline
			end-frame :newline)
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
			0 argn :key cons exit-frame end-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame :newline)
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
			100 0 argn 0 1 lookup f exit-frame end-frame :newline
			end-frame :newline
			end-frame :newline
			end-frame :newline)
                     "can find arguments after an arbitray stack size increase")
                    ((/ 1 (/ 1 (/ 1 e)))
                     ;;(e 1 / 1 / 1 /)
		     (1 e / :newline
			inner-frame "(" ?R0 ")" :newline
			1 0 argn / :newline
			inner-frame "(" ?R1 ")" :newline
			1 0 argn / exit-frame end-frame :newline
			end-frame :newline)
		     "doubly nested, division operands ordered")
		    ((progn 1 (* 2 3) 4)
		     (3 2 * :newline
			inner-frame "(" ?R ")" :newline
			1 0 argn 4 exit-frame end-frame :newline)
		     "progn executes each arg, returns the last")
		    ((if (> x y) x y)
		     ;;(x y > IF x ELSE y THEN)
		     (x y > :newline
			inner-frame "(" TEST ")" :newline
			0 argn IF :newline
			x exit-frame :newline
			ELSE :newline
			y exit-frame :newline
			THEN :newline
			end-frame :newline)
		     "top level if")
		    ((f (if (> x y) x y))
		     ;;(x y > IF x f ELSE y f THEN)
		     (x y > :newline
			inner-frame "(" TEST ")" :newline
			0 argn IF :newline
			x f exit-frame :newline
			ELSE :newline
			y f exit-frame :newline
			THEN :newline
			end-frame :newline)
		     "if as an arg")
                    ((defun fn () 123)
                     (":" fn "(" ")" :newline
                      begin-frame :newline
                      123
                      exit-frame end-frame :newline
                      ";")
                     "defun returning a value")
                    ((defun fn (x y) (+ x y))
                     (":" fn "(" y x ")" :newline
                      begin-frame :newline
                      1 argn 0 argn +
                      exit-frame end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun mag (x y) (+ (* x x) (* y y)))
                     (":" mag "(" y x ")" :newline
                      begin-frame :newline
                      0 argn 0 argn * :newline
		      inner-frame "(" ?R0 ")" :newline
                      1 1 lookup 1 1 lookup * :newline
		      inner-frame "(" ?R1 ")" :newline
                      0 argn 0 1 lookup + exit-frame end-frame :newline
		      end-frame :newline
		      end-frame :newline
                      ";")
                     "defun with one call")
                    ((defun fn (x) (- 2 (if (> x 3) 3 4)))
                     (":" FN "(" X ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
                      0 ARGN 3 > :NEWLINE
                      INNER-FRAME "(" TEST ")" :NEWLINE
                      0 ARGN IF :NEWLINE
                      2 3 - EXIT-FRAME :NEWLINE
		      ELSE :NEWLINE
                      2 4 - EXIT-FRAME :NEWLINE
		      THEN :NEWLINE
                      END-FRAME :newline
		      END-FRAME :newline
                      ";")
                     "subtraction and comparisons get swapped")
                    ((defun squarer () (lambda (x) (* x x)))
                     (":" squarer "(" ")" :newline
                      begin-frame :newline
                      "[" begin-frame "(" x ?cc ")" :newline
                      1 argn 1 argn *
                      exit-frame end-frame :newline
                      "]" close-lambda exit-frame end-frame :newline
                      ";")
                     "function returning an anonymous function")
		    ((defun f (x) (lambda (a) (+ x y a)))
		     (":" f "(" x ")" :newline
		      begin-frame :newline
		      "[" begin-frame "(" a ?cc ")" :newline
		      1 argn y 0 0 closure-lookup + exit-frame end-frame :newline
		      "]" close-lambda exit-frame end-frame :newline
		      ";")
		     "lambda with variable from caller")
		    ((mapcar (lambda (i) (* i i)) lst)
		     ("[" begin-frame "(" i ?cc ")" :newline
		      1 argn 1 argn * exit-frame end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?R ")" :newline
		      lst 0 argn mapcar exit-frame end-frame :newline)
		     "lambda as an argument")
		    (((lambda (x) (who x)) 3 4 5)
		     ("[" begin-frame "(" x ?cc ")" :newline
		      1 argn who exit-frame end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?RL ")" :newline
		      5 4 3 0 argn exec exit-frame end-frame :newline)
		     "lambda in first position")
		    ((let ((x (+ 2 3))
			   (y (* 2 2)))
		       (+ x y))
		     ("[" begin-frame "(" y x ?CC ")" :newline
		      2 argn 1 argn + exit-frame end-frame :newline
		      "]" close-lambda :newline
		      inner-frame "(" ?RL ")" :newline
		      3 2 + :newline
		      inner-frame "(" ?RX ")" :newline
		      2 2 * :newline
		      inner-frame "(" ?RY ")" :newline
		      0 argn 0 1 lookup 0 2 lookup funcall exit-frame end-frame :newline
		      end-frame :newline
		      end-frame :newline)
		     "let forms"))
                  :fn #'nolisp:compile-form
                  :allow-keywords nil
		  ))

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
				    exit-frame :newline
				    end-frame :newline
				    "]" current-frame close-lambda :newline
				    exit-frame end-frame :newline
				    ";")
		": SQUARER ( X )
BEGIN-FRAME
[ BEGIN-FRAME ( X )
0 ARGN 0 ARGN * EXIT-FRAME
END-FRAME
] CURRENT-FRAME CLOSE-LAMBDA
EXIT-FRAME END-FRAME
;"))
		#'nolisp:to-string))

(defun test-compile-to-string ()
  (let ((*gensym-counter* 1000))
    (assert-equal (nolisp:compile-to-string '(defun f (x y) (+ (* x x) (* y y))))
		  ": F ( Y X )
BEGIN-FRAME
0 ARGN 0 ARGN *
INNER-FRAME ( R1000 )
1 1 LOOKUP 1 1 LOOKUP *
INNER-FRAME ( R1001 )
0 ARGN 0 1 LOOKUP + EXIT-FRAME END-FRAME
END-FRAME
END-FRAME
;")))

(defun test-compile ()
  (test-compile-form)
  (test-to-string)
  (test-compile-to-string)
  t)
