;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(require "bacaw")

(defun print-disassembly (sexp cs toplevel &optional (n 0))
  ;; function addresses are stored in DS which is unavailable until more data is stored about the toplevel
  ;; (format *standard-output* "~D: ~A ~A ~A~%" n (first sexp) (assoc (+ n cs) toplevel) (+ n cs))
  (format *standard-output* "~D: ~A~%" n (first sexp))
  (if (rest sexp)
      (print-disassembly (rest sexp) cs toplevel (+ 1 n))))

(defun toplevel-strings (toplevel strings &optional acc)
  (if toplevel
      (let ((item (first toplevel)))
        (toplevel-strings (rest toplevel)
                          strings
                          (cons (cons (first (first toplevel))
                                      (second (assoc (cdr (first toplevel)) strings)))
                                acc)))
      acc))

(defun print-toplevel (toplevel-symbols)
  (print (first toplevel-symbols))
  (if toplevel-symbols
      (print-toplevel (rest toplevel-symbols))))

(defun disassemble-asm (seq)
  (multiple-value-bind (ops cs strings toplevel)
      (repl::bacaw-isa-disassemble seq)
    (let ((toplevel-symbols (toplevel-strings toplevel strings)))
      (print-disassembly ops cs toplevel-symbols)
      (print-toplevel toplevel-symbols)
      (print strings))))


