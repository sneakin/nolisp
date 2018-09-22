;;; -*- mode: Lisp; coding: utf-8-unix -*-
(require "memory")
(require "symbol")
(require "emitter")
(require "compiler/package")

(in-package :repl)

(defvar *ISR-MAX* 128)
(defvar *ISR-BYTE-SIZE* (* *REGISTER-SIZE* 2))
(defvar *ISR-TOTAL-SIZE* (* *ISR-BYTE-SIZE* *ISR-MAX*))

(defun write-toplevel (output toplevel-start toplevel string-segment)
  (if (<= toplevel-start toplevel)
      (write-toplevel (ptr-write-long (- (ptr-read-ulong toplevel-start) string-segment) output)
                      (+ toplevel-start *SIZEOF_LONG*)
                      toplevel
                      string-segment)))

(defun write-to-array (output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel)
  ;; create reset ISR that init's CS, DS, calls init, and toplevel's init in asm-stack
  ;; todo CS & DS can be anywhere, offset in file is good
  (format *standard-output* "write-to-array ~A ~A ~A~%" (ptr-read-array asm-start (- asm-stack asm-start)) (- token-offset token-start) (- code-segment cs-start))
  (multiple-value-bind (init-sym token-offset)
      (symbol-intern *INIT-FUNC* token-start token-offset)
    (let* ((toplevel (env-define token-offset toplevel-start toplevel))
           (asm-stack-end (emit-init asm-stack *ISR-TOTAL-SIZE* 2048 toplevel-start toplevel init-sym))
           (code-segment-end (ptr-copy asm-start code-segment (- asm-stack-end asm-start)))
           (isr-end (emit-integer (emit-op asm-stack :load 12 0 15) (+ *ISR-TOTAL-SIZE* (- code-segment cs-start))))
           )
      ;; zero ISR
      (ptr-zero output *ISR-TOTAL-SIZE*)
      ;; place jump at byte 0
      (ptr-copy asm-stack output (- isr-end asm-stack))
      ;; code segment
      (ptr-copy cs-start (+ output *ISR-TOTAL-SIZE*) (- code-segment-end cs-start))
      ;; strings
      (ptr-copy token-start (+ output *ISR-TOTAL-SIZE* (- code-segment-end cs-start)) (- token-offset token-start))
      ;; toplevel symbol table
      ;; todo subtract DS from the toplevel addresses
      (write-toplevel (+ output *ISR-TOTAL-SIZE* (- code-segment-end cs-start) (- token-offset token-start)) toplevel-start toplevel token-start)
      ;; write indexes: code-segment strings toplevel
      (ptr-write-long (+ *ISR-TOTAL-SIZE*
                         (- code-segment-end cs-start)
                         (- token-offset token-start))
                      (ptr-write-long (+ *ISR-TOTAL-SIZE*
                                         (- code-segment-end cs-start))
                                      (ptr-write-long (+ *ISR-TOTAL-SIZE*)
                                                      (+ output *ISR-TOTAL-SIZE*
                                                         (- code-segment-end cs-start)
                                                         (- token-offset token-start)
                                                         (- toplevel toplevel-start)))))))
  )
