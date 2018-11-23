;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "runtime/bc/io/console")
(require "runtime/interrupts")
(require "runtime/halt")

(console-write "Disabling Interrupts")
(console-write-integer (interrupts-offset 1))
(console-write-integer (interrupts-offset 2))
(interrupts-disable)

(console-write "Installing Interrupt")
(interrupts-install 0 (isr
                       (values #xAA #xBB)
                       (asm (halt))))
(interrupts-install 1 (lambda ()
                        (console-write "Interrupted 1!")
                        (interrupts-return)))
(interrupts-install 3 (isr
                        (console-write "Interrupted 3!")))
(interrupts-install 9 (isr
                        (console-write "Interrupted by keyboard!")))
(interrupts-install 10 (isr
                        (console-write "Interrupted by gfx!")))
(interrupts-install 128 (isr
                       (let ((x (+ 1 2)))
                         (console-write "Interrupted 128!")
                         (console-write-integer x))
                        (wakeup)))
(define-isr handler
  (console-write "Interrupted 2!"))
(interrupts-install 2 handler)
(console-write-unsigned-integer handler 16)
(console-write-unsigned-integer (+ handler (code-segment)) 16)

(console-write-unsigned-integer (ptr-read-ulong (interrupts-offset 0)) 16)
(console-write-unsigned-integer (ptr-read-ulong (+ (interrupts-offset 0) 4)) 16)
(console-write-unsigned-integer (ptr-read-ulong (interrupts-offset 1)) 16)
(console-write-unsigned-integer (ptr-read-ulong (+ (interrupts-offset 1) 4)) 16)
(console-write-unsigned-integer (ptr-read-ulong (interrupts-offset 2)) 16)
(console-write-unsigned-integer (ptr-read-ulong (+ (interrupts-offset 2) 4)) 16)

(console-write "Interrupts installed")
(interrupts-enable) ;; triggers a reset due to pending interrupts
(console-write "Interrupts enabled")

(values (ptr-read-ulong (interrupts-offset 1))
        (ptr-read-ulong (+ (interrupts-offset 1) 4)))
                                        ;(interrupt 3)
(interrupt 2)
(asm (int 128))
(console-write "Sleep time")
(asm (sleep))
(console-write "Wakey wakey")
(asm (reset))
