(defun test-to-string ()
  (assert-cases '((((hello world)) "HELLO WORLD")
		  (((hello nil world nil)) "HELLO NIL WORLD NIL")
		  (((hello :call world :var)) "HELLO WORLD")
		  (((:var hello :call world)) "HELLO WORLD")
		  (((hello :call world :var 123)) "HELLO WORLD 123")
		  (((hello :nonexit world)) "HELLO WORLD")
		  (((hello :newline world)) "HELLO
WORLD")
		  (((hello "world")) "HELLO world")
		  ((("hello" world)) "hello WORLD")
		  (((hello "\"world\"")) "HELLO \"world\"")
		  (((":" squarer "(" x ")" :newline
		    begin-frame :newline
		    "[" begin-frame "(" x ")" :newline
		    0 argn 0 argn *
		    exit-frame :newline
		    end-frame :newline
		    "]" current-frame close-lambda :newline
		    exit-frame end-frame :newline
		    ";"))
		   ": SQUARER ( X )
BEGIN-FRAME
[ BEGIN-FRAME ( X )
0 ARGN 0 ARGN * EXIT-FRAME
END-FRAME
] CURRENT-FRAME CLOSE-LAMBDA
EXIT-FRAME END-FRAME
;"))
		#'nolisp:to-string))
