(require "memory")
(require "symbol")
(require "emitter")

(in-package :repl)

(defvar *ISR-MAX* 128)
(defvar *ISR-BYTE-SIZE* (* *REGISTER-SIZE* 2))
(defvar *ISR-TOTAL-SIZE* (* *ISR-BYTE-SIZE* *ISR-MAX*))

(defun write-to-array (output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel)
  ;; create reset ISR that init's CS, DS, calls init, and toplevel's init in asm-stack
  ;; todo CS & DS can be anywhere, offset in file is good
  (format *standard-output* "write-to-array ~A ~A ~A~%" (ptr-read-array asm-start (- asm-stack asm-start)) (- token-offset token-start) (- code-segment cs-start))
  (let* ((init-sym (symbol-intern *INIT-FUNC* token-start token-offset))
         (toplevel (env-define init-sym toplevel-start toplevel))
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
    (ptr-copy toplevel-start (+ output *ISR-TOTAL-SIZE* (- code-segment-end cs-start) (- token-offset token-start)) (- toplevel toplevel-start))
    ;; write indexes: code-segment strings toplevel
    (ptr-write-long (+ output *ISR-TOTAL-SIZE*
                       (- code-segment-end cs-start)
                       (- token-offset token-start))
                    (ptr-write-long (+ output *ISR-TOTAL-SIZE*
                                       (- code-segment-end cs-start))
                                    (ptr-write-long (+ output *ISR-TOTAL-SIZE*)
                                                    (+ output *ISR-TOTAL-SIZE*
                                                       (- code-segment-end cs-start)
                                                       (- token-offset token-start)
                                                       (- toplevel toplevel-start)))))))
