;;;
;;; Error conditions
;;;

(in-package :nolisp)

(define-condition nc-unknown-error (nc-error) ())
(define-condition nc-macro-exists-error (nc-error) ())
(define-condition nc-undefined-error (nc-error) ())
