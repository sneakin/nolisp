(DEFUN IMPORTS-LIST-SLOT-OFFSET (SLOT)
  (COND ((EQ SLOT 'NEXT-OFFSET) 8) ((EQ SLOT 'BUFFER) 4)
        ((EQ SLOT 'MAX-SIZE) 0)
        (T (ERROR 'SLOT-ERROR :SLOT SLOT :STRUCT 'IMPORTS-LIST))))
(DEFUN IMPORTS-LIST-SIZE () 12)
(DEFUN IMPORTS-LIST-NEXT-OFFSET-REF (OBJ)
  (+ OBJ (IMPORTS-LIST-SLOT-OFFSET 'NEXT-OFFSET)))
(DEFUN IMPORTS-LIST-NEXT-OFFSET (OBJ)
  (PTR-READ-LONG (IMPORTS-LIST-NEXT-OFFSET-REF OBJ)))
(DEFUN SET-IMPORTS-LIST-NEXT-OFFSET (OBJ VALUE)
  (PTR-WRITE-LONG VALUE (IMPORTS-LIST-NEXT-OFFSET-REF OBJ))
  OBJ)
(DEFUN IMPORTS-LIST-BUFFER-REF (OBJ) (+ OBJ (IMPORTS-LIST-SLOT-OFFSET 'BUFFER)))
(DEFUN IMPORTS-LIST-BUFFER (OBJ) (PTR-READ-LONG (IMPORTS-LIST-BUFFER-REF OBJ)))
(DEFUN SET-IMPORTS-LIST-BUFFER (OBJ VALUE)
  (PTR-WRITE-LONG VALUE (IMPORTS-LIST-BUFFER-REF OBJ))
  OBJ)
(DEFUN IMPORTS-LIST-MAX-SIZE-REF (OBJ)
  (+ OBJ (IMPORTS-LIST-SLOT-OFFSET 'MAX-SIZE)))
(DEFUN IMPORTS-LIST-MAX-SIZE (OBJ)
  (PTR-READ-LONG (IMPORTS-LIST-MAX-SIZE-REF OBJ)))
(DEFUN SET-IMPORTS-LIST-MAX-SIZE (OBJ VALUE)
  (PTR-WRITE-LONG VALUE (IMPORTS-LIST-MAX-SIZE-REF OBJ))
  OBJ)
(DEFUN COPY-IMPORTS-LIST (SRC DEST)
  (PTR-COPY SRC DEST (IMPORTS-LIST-SIZE))
  DEST)
(DEFUN IMPORTS-LIST-INIT (OBJ &OPTIONAL MAX-SIZE BUFFER NEXT-OFFSET)
  (SET-IMPORTS-LIST-MAX-SIZE OBJ MAX-SIZE)
  (SET-IMPORTS-LIST-BUFFER OBJ BUFFER)
  (SET-IMPORTS-LIST-NEXT-OFFSET OBJ NEXT-OFFSET)
  OBJ)
(DEFUN IMPORT-ENTRY-SLOT-OFFSET (SLOT)
  (COND ((EQ SLOT 'FUN-NAME) 4) ((EQ SLOT 'LIB) 0)
        (T (ERROR 'SLOT-ERROR :SLOT SLOT :STRUCT 'IMPORT-ENTRY))))
(DEFUN IMPORT-ENTRY-SIZE () 8)
(DEFUN IMPORT-ENTRY-FUN-NAME-REF (OBJ)
  (+ OBJ (IMPORT-ENTRY-SLOT-OFFSET 'FUN-NAME)))
(DEFUN IMPORT-ENTRY-FUN-NAME (OBJ)
  (PTR-READ-POINTER (IMPORT-ENTRY-FUN-NAME-REF OBJ)))
(DEFUN SET-IMPORT-ENTRY-FUN-NAME (OBJ VALUE)
  (PTR-WRITE-POINTER VALUE (IMPORT-ENTRY-FUN-NAME-REF OBJ))
  OBJ)
(DEFUN IMPORT-ENTRY-LIB-REF (OBJ) (+ OBJ (IMPORT-ENTRY-SLOT-OFFSET 'LIB)))
(DEFUN IMPORT-ENTRY-LIB (OBJ) (PTR-READ-POINTER (IMPORT-ENTRY-LIB-REF OBJ)))
(DEFUN SET-IMPORT-ENTRY-LIB (OBJ VALUE)
  (PTR-WRITE-POINTER VALUE (IMPORT-ENTRY-LIB-REF OBJ))
  OBJ)
(DEFUN COPY-IMPORT-ENTRY (SRC DEST)
  (PTR-COPY SRC DEST (IMPORT-ENTRY-SIZE))
  DEST)
(DEFUN IMPORT-ENTRY-INIT (OBJ &OPTIONAL LIB FUN-NAME)
  (SET-IMPORT-ENTRY-LIB OBJ LIB)
  (SET-IMPORT-ENTRY-FUN-NAME OBJ FUN-NAME)
  OBJ)
