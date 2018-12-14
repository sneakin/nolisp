;;; -*- mode: Lisp; coding: utf-8-unix -*-
(require "memory")
(require "symbol")
(require "cpu/bacaw/emitter")
(require "compiler/package")
(require "logging")

(in-package :repl)

(defvar *ISR-MAX* 128)
(defvar *ISR-BYTE-SIZE* (* *REGISTER-SIZE* 2))
(defvar *ISR-TOTAL-SIZE* (* *ISR-BYTE-SIZE* *ISR-MAX*))
(defvar *INIT-SIZE* 1024)

(defun write-toplevel (output toplevel-start toplevel string-segment data-segment)
  (if (<= toplevel-start toplevel)
      (write-toplevel (ptr-write-long (- (ptr-read-ulong toplevel-start) string-segment) output)
                      (+ toplevel-start *SIZEOF_LONG*)
                      toplevel
                      string-segment
                      data-segment)
      output))

(defun define-toplevel-symbols (token-start token-offset)
  (multiple-value-bind (init-sym token-offset)
      (symbol-intern *INIT-FUNC* token-start token-offset)
    (multiple-value-bind (string-segment token-offset)
        (symbol-intern "*string-segment*" token-start token-offset)
      (values init-sym string-segment token-offset))))

(defun write-to-array (output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel &optional (data-segment-offset nil))
  ;; create reset ISR that init's CS, DS, calls init, and toplevel's init in asm-stack
  ;; todo CS & DS can be anywhere, offset in file is good
  (multiple-value-bind (init-sym string-segment-sym token-offset)
      (define-toplevel-symbols token-start token-offset)
    (let* ((toplevel (env-define token-offset toplevel-start toplevel))
           ;; todo need to get the correct offset after init is emitted
           (string-segment (+ *INIT-SIZE* *ISR-TOTAL-SIZE* (- code-segment cs-start)))
           (data-segment (align-bytes (or data-segment-offset (+ string-segment (- token-offset token-start))) 4096)))
      (logger :debug "write-to-array ~A ~A ~A ~A ~A~%" (ptr-read-array asm-start (- asm-stack asm-start)) (- token-offset token-start) (- code-segment cs-start) string-segment data-segment)
      (multiple-value-bind (asm-stack-end string-segment-jump-offset)
          (emit-init asm-stack
                     *ISR-TOTAL-SIZE*
                     data-segment
                     toplevel-start
                     toplevel
                     code-segment
                     init-sym
                     string-segment-sym)
        (let* ((init-size (- asm-stack-end asm-start))
               (code-segment-end (ptr-copy asm-start
                                           code-segment
                                           init-size))
               (init-offset (+ *ISR-TOTAL-SIZE* (- code-segment cs-start)))
               (isr-end (emit-integer (emit-op asm-stack :load 12 0 15)
                                      init-offset))
               (code-segment-size (+ *ISR-TOTAL-SIZE* (- code-segment-end cs-start)))
               (total-size (+ *ISR-TOTAL-SIZE*
                              (- code-segment-end cs-start)
                              (- token-offset token-start)
                              (- toplevel toplevel-start)
                              (* 4 *SIZEOF_LONG*))))
          ;; fix string-segment offset
          (ptr-write-long code-segment-size (+ code-segment (- string-segment-jump-offset asm-start)))
          (logger :debug
                  ";; setting string-segment at ~A -> ~A~%"
                  (- string-segment-jump-offset asm-start)
                  (+ code-segment (- string-segment-jump-offset asm-start)))
          (logger :debug
                  ";; code-segment at ~A:~A ~A: ~A~%"
                  cs-start code-segment code-segment-size (ptr-read-array cs-start 64))
          (logger :debug
                  ";; token-segment at ~A:~A~%"
                  token-start (- token-offset token-start))
          (logger :info "Total size ~d bytes~%" total-size)
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
          ;  (write-toplevel (+ output *ISR-TOTAL-SIZE* (- code-segment-end cs-start) (- token-offset token-start)) toplevel-start toplevel token-start)
          (write-toplevel (+ output *ISR-TOTAL-SIZE* (- code-segment-end cs-start) (- token-offset token-start)) toplevel-start toplevel token-start data-segment-offset)
          ;; write indexes: code-segment strings toplevel total-size
          (ptr-write-long total-size
                          (ptr-write-long (+ *ISR-TOTAL-SIZE*
                                             (- code-segment-end cs-start)
                                             (- token-offset token-start))
                                          (ptr-write-long (+ *ISR-TOTAL-SIZE*
                                                             (- code-segment-end cs-start))
                                                          (ptr-write-long (+ *ISR-TOTAL-SIZE*)
                                                                          (+ output *ISR-TOTAL-SIZE*
                                                                             (- code-segment-end cs-start)
                                                                             (- token-offset token-start)
                                                                             (- toplevel toplevel-start)))))))))))


