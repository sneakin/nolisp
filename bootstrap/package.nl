;;; -*- mode: Lisp; coding: utf-8-unix -*-

(DEFUN PACKAGE-SLOT-OFFSET (SLOT)
  (COND ((EQ SLOT 'SOURCE-FILES) 36) ((EQ SLOT 'SYMBOLS) 24)
        ((EQ SLOT 'STRING-SEGMENT) 12) ((EQ SLOT 'CODE-SEGMENT) 0)
        (T (ERROR 'SLOT-ERROR :SLOT SLOT :STRUCT 'PACKAGE))))
(DEFUN PACKAGE-SIZE () 48)
(DEFUN PACKAGE-SOURCE-FILES-REF (OBJ)
  (+ OBJ (PACKAGE-SLOT-OFFSET 'SOURCE-FILES)))
(DEFUN PACKAGE-SOURCE-FILES (OBJ) (PACKAGE-SOURCE-FILES-REF OBJ))
(DEFUN SET-PACKAGE-SOURCE-FILES (OBJ VALUE)
  (IF VALUE
      (PTR-COPY VALUE (PACKAGE-SOURCE-FILES-REF OBJ) 12)
      (PTR-ZERO (PACKAGE-SOURCE-FILES-REF OBJ) 12))
  OBJ)
(DEFUN PACKAGE-SYMBOLS-REF (OBJ) (+ OBJ (PACKAGE-SLOT-OFFSET 'SYMBOLS)))
(DEFUN PACKAGE-SYMBOLS (OBJ) (PACKAGE-SYMBOLS-REF OBJ))
(DEFUN SET-PACKAGE-SYMBOLS (OBJ VALUE)
  (IF VALUE
      (PTR-COPY VALUE (PACKAGE-SYMBOLS-REF OBJ) 12)
      (PTR-ZERO (PACKAGE-SYMBOLS-REF OBJ) 12))
  OBJ)
(DEFUN PACKAGE-STRING-SEGMENT-REF (OBJ)
  (+ OBJ (PACKAGE-SLOT-OFFSET 'STRING-SEGMENT)))
(DEFUN PACKAGE-STRING-SEGMENT (OBJ) (PACKAGE-STRING-SEGMENT-REF OBJ))
(DEFUN SET-PACKAGE-STRING-SEGMENT (OBJ VALUE)
  (IF VALUE
      (PTR-COPY VALUE (PACKAGE-STRING-SEGMENT-REF OBJ) 12)
      (PTR-ZERO (PACKAGE-STRING-SEGMENT-REF OBJ) 12))
  OBJ)
(DEFUN PACKAGE-CODE-SEGMENT-REF (OBJ)
  (+ OBJ (PACKAGE-SLOT-OFFSET 'CODE-SEGMENT)))
(DEFUN PACKAGE-CODE-SEGMENT (OBJ) (PACKAGE-CODE-SEGMENT-REF OBJ))
(DEFUN SET-PACKAGE-CODE-SEGMENT (OBJ VALUE)
  (IF VALUE
      (PTR-COPY VALUE (PACKAGE-CODE-SEGMENT-REF OBJ) 12)
      (PTR-ZERO (PACKAGE-CODE-SEGMENT-REF OBJ) 12))
  OBJ)
(DEFUN COPY-PACKAGE (SRC DEST) (PTR-COPY SRC DEST (PACKAGE-SIZE)) DEST)
(DEFUN PACKAGE-INIT
    (OBJ &OPTIONAL CODE-SEGMENT STRING-SEGMENT SYMBOLS SOURCE-FILES)
  (SET-PACKAGE-CODE-SEGMENT OBJ CODE-SEGMENT)
  (SET-PACKAGE-STRING-SEGMENT OBJ STRING-SEGMENT)
  (SET-PACKAGE-SYMBOLS OBJ SYMBOLS)
  (SET-PACKAGE-SOURCE-FILES OBJ SOURCE-FILES)
  OBJ)
