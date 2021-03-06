;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/output-dev")
(require "runtime/halt")
(require "runtime/itoa")
(require "runtime/io")

(var *banner* "WELCOME!!")

(output-dev-write 'hello-world)
(output-dev-write "\r\n\r\n")
(output-dev-write *banner*)
(output-dev-write-integer -123)
(with-allocation (str 48)
  (output-dev-write "\r\nLog2:\t")
  (output-dev-write-integer (log2-unsigned 1234))
  (output-dev-write "\r\nLog3:\t")
  (output-dev-write (itoa (log 1234 3) str 10))
  (output-dev-write "\r\nMax digit:\t")
  (output-dev-write-integer (count-digits-brute 1234 3))
  (output-dev-write "\r\nMax digit guess:\t")
  (output-dev-write-integer (count-digits-guesser 1234 3))
  (output-dev-write "\r\n\r\nDec:\t")
  (output-dev-write (itoa 1234 str))
  (output-dev-write "\r\nHex:\t")
  (output-dev-write (itoa #x-1234 str 16))
  (output-dev-write "\r\nOct:\t")
  (output-dev-write (itoa #x-1234 str 8))
  (output-dev-write "\r\nBin:\t")
  (output-dev-write (itoa #x-1234 str 2))
  (output-dev-write "\r\nTern:\t")
  (output-dev-write (itoa #x-1234 str 3))
  (output-dev-write "\r\nThirteen:\t")
  (output-dev-write (itoa #x-1234 str 13))
  (output-dev-write "\r\n26:\t")
  (output-dev-write (itoa #x-1234 str 26))
  (output-dev-write "\r\n52:\t")
  (output-dev-write (itoa #x-1234 str 52))
  (output-dev-write "\r\n64:\t")
  (output-dev-write (itoa #x-1234 str 64))
  (output-dev-write "\r\nBin FFFF:\t")
  (output-dev-write (itoa-unsigned #xFFFF str 2))
  (output-dev-write "\r\nBin -FFFF:\t")
  (output-dev-write-integer (count-digits-brute #x-FFFF 2))
  (output-dev-write "\r\n\t")
  (output-dev-write (itoa-unsigned #x-FFFF str 2))
  (output-dev-write "\r\n\t")
  (output-dev-write (itoa-unsigned #x-1 str 2))
  (output-dev-write "\r\n"))
(output-dev-write "Format test:\r\n")
(cformat *standard-output* "%s %S, Formatting %i with %%i and %x with %%x and %c with %%c.\r\n" "You" (upcase "You") 123 #x123 #x50)
(cformat *standard-output* "Done\r\n")
(format *standard-output* "~S ~S, Formatting ~i with ~~i and ~x with ~~x and ~c with ~~c.~%" "You" (upcase "You") 123 #x123 #x50)
(format *standard-output* "Done~%")
(output-dev-write "\r\nGood bye...\r\n")

(halt)
