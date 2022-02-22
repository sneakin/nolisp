(defun test-toplevel ()
  (let ((*gensym-counter* 1000))
    (assert-cases '((((2 (sq 3))) "2
3 SQ
" "simple forms")
		    ((((defun f (x) (+ x 2)))) ": F ( X )
BEGIN-FRAME
2 0 ARGN + EXIT-FRAME
END-FRAME
;
" "single defun")
		    ((((defun f (x) (+ x 2)) (defun g (x) (+ x (f x)))))
		     ": F ( X )
BEGIN-FRAME
2 0 ARGN + EXIT-FRAME
END-FRAME
;
: G ( X )
BEGIN-FRAME
0 ARGN F
INNER-FRAME ( R1003 )
0 ARGN 0 1 LOOKUP + EXIT-FRAME
END-FRAME
END-FRAME
;
" "two defuns")
		    ((((defvar x (+ 2 3)))) "3 2 +
INNER-FRAME ( R1006 )
0 ARGN VAR> X
END-FRAME
" "variables")
		    ((((defconstant x (+ 2 4)))) "4 2 +
INNER-FRAME ( R1009 )
0 ARGN CONST> X
END-FRAME
" "constants")
		    ((((defmacro m (x) `(* ,x ,x)))) "" "macro definition")
		    ((((defmacro m (x) `(* ,x ,x)) (m 3))) "3 3 *
" "macro call")
		    ((((defmacro m (n i) `(defun ,n (x) (* ,i x)))
		      (m f 3))) ": F ( X )
BEGIN-FRAME
0 ARGN 3 * EXIT-FRAME
END-FRAME
;
" "macro to defun"))
		  #'nolisp::toplevel-compile)))
