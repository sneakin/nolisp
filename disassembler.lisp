;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(require "cpu/bacaw/bacaw")

(defun print-disassembly (sexp cs toplevel &optional (n 0))
  ;; function addresses are stored in DS which is unavailable until more data is stored about the toplevel
  ;; (format *standard-output* "~D: ~A ~A ~A~%" n (first sexp) (assoc (+ n cs) toplevel) (+ n cs))
  (format *standard-output* "~D: ~A~%" n (first sexp))
  (if (rest sexp)
      (print-disassembly (rest sexp) cs toplevel (+ 1 n))))

(defun toplevel-strings (toplevel strings &optional acc)
  (if toplevel
      (let ((item (first toplevel)))
        ;; (format *standard-output* "TL: ~A~%" item)
        (toplevel-strings (rest toplevel)
                          strings
                          (cons (cons (first (first toplevel))
                                      (second (find-if #'(lambda (str) (eq (first str) (rest item)))
                                                       strings)))
                                acc)))
      acc))

(defun print-toplevel (toplevel-symbols strings)
  (if toplevel-symbols
      (let ((item (first toplevel-symbols)))
        (format *standard-output* "~A: ~A~%" (first item) (rest item))
        (print-toplevel (rest toplevel-symbols) (rest strings)))))

(defun disassemble-asm (seq)
  (multiple-value-bind (ops cs strings toplevel)
      (repl::bacaw-isa-disassemble seq)
    (let ((toplevel-symbols (toplevel-strings toplevel strings)))
      (format *standard-output* "~%Disassembly~%")
      (print-disassembly ops cs toplevel-symbols)
      (format *standard-output* "~%Toplevel~%")
      (print-toplevel toplevel-symbols strings)
      (format *standard-output* "~%Strings~%")
      (print strings))))


