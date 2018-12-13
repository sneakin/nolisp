(PROGN
 (DEFUN BYTE-BUFFER-SLOT-OFFSET (SLOT)
   (COND ((EQ SLOT 'POSITION) 8) ((EQ SLOT 'LENGTH) 4) ((EQ SLOT 'DATA) 0)
         (T (ERROR 'SLOT-ERROR :SLOT SLOT :STRUCT 'BYTE-BUFFER))))
 (DEFUN BYTE-BUFFER-SIZE () 12)
 (DEFUN BYTE-BUFFER-POSITION-REF (OBJ)
   (+ OBJ (BYTE-BUFFER-SLOT-OFFSET 'POSITION)))
 (DEFUN BYTE-BUFFER-POSITION (OBJ)
   (PTR-READ-LONG (BYTE-BUFFER-POSITION-REF OBJ)))
 (DEFUN SET-BYTE-BUFFER-POSITION (OBJ VALUE)
   (PTR-WRITE-LONG VALUE (BYTE-BUFFER-POSITION-REF OBJ))
   OBJ)
 (DEFUN BYTE-BUFFER-LENGTH-REF (OBJ) (+ OBJ (BYTE-BUFFER-SLOT-OFFSET 'LENGTH)))
 (DEFUN BYTE-BUFFER-LENGTH (OBJ) (PTR-READ-LONG (BYTE-BUFFER-LENGTH-REF OBJ)))
 (DEFUN SET-BYTE-BUFFER-LENGTH (OBJ VALUE)
   (PTR-WRITE-LONG VALUE (BYTE-BUFFER-LENGTH-REF OBJ))
   OBJ)
 (DEFUN BYTE-BUFFER-DATA-REF (OBJ) (+ OBJ (BYTE-BUFFER-SLOT-OFFSET 'DATA)))
 (DEFUN BYTE-BUFFER-DATA (OBJ) (PTR-READ-LONG (BYTE-BUFFER-DATA-REF OBJ)))
 (DEFUN SET-BYTE-BUFFER-DATA (OBJ VALUE)
   (PTR-WRITE-LONG VALUE (BYTE-BUFFER-DATA-REF OBJ))
   OBJ)
 (DEFUN COPY-BYTE-BUFFER (SRC DEST)
   (PTR-COPY SRC DEST (BYTE-BUFFER-SIZE))
   DEST)
 (DEFUN BYTE-BUFFER-INIT (OBJ &OPTIONAL DATA (LENGTH 0) (POSITION 0))
   (SET-BYTE-BUFFER-DATA OBJ DATA)
   (SET-BYTE-BUFFER-LENGTH OBJ LENGTH)
   (SET-BYTE-BUFFER-POSITION OBJ POSITION)
   OBJ))