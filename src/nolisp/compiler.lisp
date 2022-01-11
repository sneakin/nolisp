;;;
;;; Compiler API
;;;

(in-package :nolisp)

(defun nc-list-compile (form)
  (nc-forthgen (nc-lookup-resolver (nc-cps-transform (nc-macroexpand form)))))

(defun nc-compile (form)
  (flatten (nc-forthgen (nc-lookup-resolver (nc-cps-transform (nc-macroexpand form))))))

(defun nc-to-string (form)
  (format nil "~{~A ~}" (substitute "" :var (substitute "" :call (substitute (format nil "~%") :newline form)))))
