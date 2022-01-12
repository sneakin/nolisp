;;;
;;; Compiler API
;;;

(in-package :nolisp)

(defun list-compile (form)
  (forthgen (lookup-resolver (cps-transform (macro-expand form)))))

(defun compile-form (form)
  (flatten (forthgen (lookup-resolver (cps-transform (macro-expand form))))))

(defun to-string (form)
  (format nil "~{~A ~}" (substitute "" :var (substitute "" :call (substitute (format nil "~%") :newline form)))))
