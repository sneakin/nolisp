;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/memory")

(defvar interrupts-isr-asm #xF0C5)
(defvar *SIZEOF-ISR* 8)

(defun interrupts-addr ()
  (asm (mov 0 13)))

(defun code-segment ()
  (asm (mov 0 10)))

(defun interrupts-offset (n)
  (+ (interrupts-addr) (* n *SIZEOF-ISR*)))

(defun interrupts-enabled? ()
  (asm (mov 0 14)
       (load 1 0 15) #x10
       (and 1)))

(defun interrupts-enable ()
  (asm (sie))
  t)

(defun interrupts-disable ()
  (asm (cie))
  t)

(defun interrupts-install (n handler)
  (let ((en (interrupts-enabled?)))
    (if en (interrupts-disable))
    (ptr-write-ushort 0 (ptr-write-ulong (+ handler (code-segment))
                                         (ptr-write-ushort interrupts-isr-asm (interrupts-offset n))))
    (if en (interrupts-enable))
    n))

(defun interrupts-return ()
  ;; todo needs to get the stack back to how it was when the interrupt was entered
  ;; call and rti in ISR?
  (asm (rti)))

(defun interrupt (n)
  (asm (load 0 0 11) 4
       (intr 0)))
