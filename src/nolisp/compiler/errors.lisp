;;;
;;; Error conditions
;;;

(in-package :nolisp)

(define-condition unknown-error (nolisp-error) ())
(define-condition macro-exists-error (nolisp-error) ())
(define-condition undefined-error (nolisp-error) ())
