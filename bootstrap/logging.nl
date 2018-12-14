(DEFCONSTANT LOGGER-LEVEL-DEBUG 5)
(DEFCONSTANT LOGGER-LEVEL-VERBOSE 4)
(DEFCONSTANT LOGGER-LEVEL-INFO 3)
(DEFCONSTANT LOGGER-LEVEL-WARNING 2)
(DEFCONSTANT LOGGER-LEVEL-FATAL 1)
(DEFCONSTANT LOGGER-LEVEL-QUIET 0)
(DEFUN LOGGER-LEVEL-NAME (#:G571)
  (COND ((EQ #:G571 5) 'LOGGER-LEVEL-DEBUG)
        ((EQ #:G571 4) 'LOGGER-LEVEL-VERBOSE)
        ((EQ #:G571 3) 'LOGGER-LEVEL-INFO)
        ((EQ #:G571 2) 'LOGGER-LEVEL-WARNING)
        ((EQ #:G571 1) 'LOGGER-LEVEL-FATAL) ((EQ #:G571 0) 'LOGGER-LEVEL-QUIET)
        (T (ERROR 'UNKNOWN-ENUM-VALUE :ENUM 'LOGGER-LEVEL :NAME #:G571))))
(DEFUN LOGGER-LEVEL-STRING (#:G572)
  (COND ((EQ #:G572 5) "DEBUG") ((EQ #:G572 4) "VERBOSE")
        ((EQ #:G572 3) "INFO") ((EQ #:G572 2) "WARNING")
        ((EQ #:G572 1) "FATAL") ((EQ #:G572 0) "QUIET")
        (T (ERROR 'UNKNOWN-ENUM-VALUE :ENUM 'LOGGER-LEVEL :NAME #:G572))))
(DEFUN LOGGER-LEVEL-NUMBER (#:G573)
  (COND ((EQ #:G573 5) 5) ((EQ #:G573 4) 4) ((EQ #:G573 3) 3) ((EQ #:G573 2) 2)
        ((EQ #:G573 1) 1) ((EQ #:G573 0) 0)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "DEBUG")) 5)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "VERBOSE")) 4)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "INFO")) 3)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "WARNING")) 2)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "FATAL")) 1)
        ((AND (STRINGP #:G573) (STRING-EQUAL #:G573 "QUIET")) 0)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "DEBUG")) 5)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "VERBOSE"))
         4)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "INFO")) 3)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "WARNING"))
         2)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "FATAL")) 1)
        ((AND (SYMBOLP #:G573) (STRING-EQUAL (SYMBOL-NAME #:G573) "QUIET")) 0)
        (T (ERROR 'UNKNOWN-ENUM-NAME :ENUM 'LOGGER-LEVEL :NAME #:G573))))
