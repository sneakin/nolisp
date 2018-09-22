;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "symbol")
(require "string")

(in-package :repl)

(defun make-short (&optional a b c d)
  (logior (or a 0)
          (ash (or b 0) 4)
          (ash (or c 0) 8)
          (ash (or d 0) 12)))

(defun make-op (op &optional a b c)
  (cond
    ((eq op :NOP) (make-short 0 0 0 0))
    ((eq op :NOT) (make-short 0 1 0 0))
    ((eq op :OR) (make-short 0 2 0 0))
    ((eq op :XOR) (make-short 0 3 0 0))
    ((eq op :AND) (make-short 0 4 0 0))
    ((eq op :BSL) (make-short 0 5 0 0))
    ((eq op :INT) (make-short 0 7 a b))
    ((eq op :HALT) (make-short 0 8 a b))
    ((eq op :NEG) (make-short 0 9 a b))
    ((eq op :RTI) (make-short 0 12 a b))
    ((eq op :BSR) (make-short 0 13 a b))
    ((eq op :CLS) (make-short 0 #xe a b))
    ((eq op :INC) (make-short #x1 a b c))
    ((eq op :ADDI) (make-short #x2 #x1 a b))
    ((eq op :SUBI) (make-short #x2 #x9 a b))
    ((eq op :MULI) (make-short #x2 #x2 a b))
    ((eq op :POWI) (make-short #x2 #x4 a b))
    ((eq op :DIVI) (make-short #x2 #xa a b))
    ((eq op :CONVI) (make-short #x2 #xb a b))
    ((eq op :CMP) (make-short #x2 0 a b))
    ((eq op :CMPI) (make-short #x2 0 a b))
    ((eq op :CEILF) (make-short #x4 #x6 a b))
    ((eq op :LOAD) (make-short #x5 a b c))
    ((eq op :POP) (make-short #x6 a b c))
    ((eq op :CALL) (make-short #x7 #x7 a b))
    ((eq op :RET) (make-short #x7 #xc a b))
    ((eq op :RESET) (make-short #x7 #x1 a b))
    ((eq op :MOV) (make-short #x8 a b c))
    ((eq op :DEC) (make-short #x9 a b c))
    ((eq op :STORE) (make-short #xD a b c))
    ((eq op :PUSH) (make-short #xE a b c))
    (t (error 'unknown-op-error :op op))))

(defun emit-op (stack op &optional a b c)
  (let ((op (if (symbolp op)
                op
                (intern (string-upcase op) "KEYWORD"))))
    (format *standard-output* "~A ~A ~A ~A    ~A~%" op (or a 0) (or b 0) (or c 0) (make-op op a b c))
    (ptr-write-short (make-op op a b c) stack)
    (+ stack *SIZEOF_SHORT*))
)

(defun emit-float (stack value)
  (format *standard-output* "float32(~A)~%" value)
  (ptr-write-float value stack)
  (+ stack *SIZEOF_FLOAT*)
  )

(defun emit-integer (stack value)
  (format *standard-output* "ulong(~A)~%" value)
  (ptr-write-long value stack)
  (+ stack *SIZEOF_LONG*)
  )

(defun emit-value (asm-stack kind value &optional (reg 0) (condition 0))
  (format *standard-output* ";; Value ~A ~A -> ~A~%" kind value reg)
  (if (eq kind 'float)
      (emit-float (emit-op asm-stack :load reg condition 15) value)
      (emit-integer (emit-op asm-stack :load reg condition 15) value)))

(defun emit-load-stack-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack :load register 0 11) (* *REGISTER-SIZE* offset))
  )

(defun emit-store-stack-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack :store register 0 11) (* *REGISTER-SIZE* offset))
  )

(defun emit-load-data-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack :load register 0 9) (* *REGISTER-SIZE* offset))
  )

(defun emit-store-data-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack :store register 0 9) (* *REGISTER-SIZE* offset))
  )

(defun emit-lookup-call (asm-stack symbol symbol-index)
  ;; load the symbol value into R1 and the toplevel lookup from the top of the stack
  ;; todo need to lookup the lookup function since lambad's set env to env-start
  (format *standard-output* ";;  globally~%")
  (emit-integer (emit-op (emit-integer (emit-op (emit-integer (emit-op asm-stack 'load 1 0 15)
                                                              symbol)
                                                :load 0 0 11)
                                       (symbol-index-position symbol-index))
                         :call 0 10)
                0))

(defun emit-lookup-global (asm-stack symbol symbol-index)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (format *standard-output* ";; Lookup global ~A ~A ~A~%" symbol (symbol-string symbol) symbol-index)
  (let ((stack-pos (symbol-index-offset symbol-index symbol)))
    (if stack-pos
        (emit-load-data-value asm-stack stack-pos))))

(defun emit-lookup-local (asm-stack symbol env-start env)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (format *standard-output* ";; Lookup local ~A ~A ~A ~A ~A~%" symbol (symbol-string symbol) env-start env (symbol-id symbol))
  (let ((stack-pos (env-stack-position symbol env-start env)))
    (if stack-pos
        (emit-load-stack-value asm-stack stack-pos))))

(defun emit-lookup-inner (asm-stack symbol env-start env toplevel)
  (let ((new-asm (emit-lookup-local asm-stack symbol env-start env)))
    (if new-asm
        new-asm
        (emit-lookup-global asm-stack symbol toplevel))))

(defun emit-lookup (asm-stack symbol env-start env toplevel)
  (let ((new-asm (emit-lookup-inner asm-stack symbol env-start env toplevel)))
    (if new-asm
        new-asm
        (progn
          (env-dump env-start env)
          (env-dump (symbol-index-buffer toplevel) (symbol-index-next-offset toplevel))
          (error 'undefined-variable-error :name symbol)))))

(defun emit-lookup-or-nil (asm-stack symbol env-start env toplevel)
  (let ((new-asm (emit-lookup-inner asm-stack symbol env-start env toplevel)))
    (if new-asm
        new-asm
        (emit-value asm-stack 'integer 0))))

(defun emit-toplevel-store-reg (asm-stack name toplevel &optional (reg 0))
  (emit-store-data-value  asm-stack
                          (symbol-index-offset toplevel name)
                          reg))

(defun emit-toplevel-store-value (asm-stack name kind value toplevel)
  (emit-toplevel-store-reg (emit-value asm-stack kind value)
                           name toplevel))

(defun emit-push (asm-stack &optional (src 0))
  (emit-op asm-stack :push src))

(defun emit-pop (asm-stack &optional (dest 0))
  (emit-op asm-stack :pop dest))

(defun emit-pushers (asm-stack n &optional (reg 0))
  (if (< reg n)
      (emit-pushers (emit-push asm-stack (+ 1 reg)) n (+ 1 reg))
      asm-stack))

(defun emit-poppers (asm-stack num-bindings)
  (format *standard-output* ";; ~A poppers~%" num-bindings)
  (if (> num-bindings 0)
      (emit-integer (emit-op asm-stack :inc 11)
                    (* num-bindings *REGISTER-SIZE*))
      asm-stack))

(defun emit-stack-alloc (asm-stack size)
  (format *standard-output* ";; Allocating ~A bytes~%" size)
  ;; shift SP, push SP
  (emit-integer (emit-op asm-stack :dec 11)
                (align-bytes size)))

(defun emit-mov (asm-stack dest src)
  (emit-op asm-stack :mov dest src))

(defun emit-stack-alloc-value (asm-stack size)
  (format *standard-output* ";; Allocating ~A bytes~%" size)
  ;; shift SP, push SP
  (emit-mov (emit-stack-alloc asm-stack size)
            0 11))

(defun emit-stack-alloc-binding (asm-stack size)
  (format *standard-output* ";; Allocating ~A bytes~%" size)
  ;; shift SP, push SP
  (emit-push (emit-stack-alloc asm-stack size)
             11))

(defun emit-stack-free (asm-stack size)
  (format *standard-output* ";; Freeing ~A bytes~%" (+ (align-bytes size) (* 1 *REGISTER-SIZE*)))
  (emit-integer (emit-op asm-stack :inc 11) (+ (align-bytes size) (* 1 *REGISTER-SIZE*))))

(defun emit-jump (asm-stack offset &optional (condition 0))
  (emit-integer (emit-op asm-stack :inc 12 condition) offset))

(defun emit-fake-jump (asm-stack offset &optional (condition 0))
  (emit-jump asm-stack offset condition))

(defun emit-fixed-jump (asm-stack new-jump-offset)
  (format *standard-output* ";; fixing body jump ~A ~A~%" (+ new-jump-offset *SIZEOF_LONG*) (- asm-stack *SIZEOF_LONG*))
  (ptr-write-long (+ new-jump-offset *SIZEOF_LONG*) (- asm-stack *SIZEOF_LONG*)))

(defun emit-zero-cmp (asm-stack reg temp-reg)
  (emit-op (emit-integer (emit-op asm-stack :load temp-reg 0 15) 0)
           :cmp reg temp-reg))

(defun emit-return (asm-stack &optional (num-bindings 0))
  ;; popping of the bindings requires storing the return value, moving the return address
  ;; to the top of the frame, popping all the bindings, and restoring the return address
  ;; and value.
  (emit-op (if (> num-bindings 0)
               (emit-poppers
                (emit-pop
                 (emit-store-stack-value
                  (emit-load-stack-value
                   (emit-push asm-stack)
                   1)
                  (+ 1 num-bindings)))
                num-bindings)
               asm-stack)
           :ret))

(defun emit-reg-call (asm-stack reg)
  (emit-integer (emit-op (emit-op (emit-op asm-stack :cls #x7)
                                  :addi 10 14)
                         :call 0 reg)
                0))

(defun emit-reg-jump (asm-stack reg)
  (emit-op (emit-op (emit-op (if (> reg 0)
                                 (emit-op asm-stack :mov reg 0)
                                 asm-stack)
                             :cls #x7)
                    :addi 10 14)
           :mov 12 0))

(defun emit-call-jump  (asm-stack reg offset)
  (emit-reg-jump (emit-integer (emit-op asm-stack :load 0 0 reg)
                               offset)
                 0))

(defun emit-call  (asm-stack reg offset)
  (emit-reg-call (emit-integer (emit-op asm-stack :load 0 0 reg)
                               offset)
                 0))

(defun emit-stack-call (asm-stack offset)
  (emit-call asm-stack 11 offset))

(defun emit-pop-values (asm-stack num)
  (if (>= num 8)
      (error 'argument-error :number num)
      (if (> num 0)
          (emit-pop-values (emit-op asm-stack :pop num) (- num 1))
          asm-stack)))

(defun emit-funcall (asm-stack reg data-offset args)
  (emit-call asm-stack reg data-offset))

(defun emit-copy-data-value (asm-stack src dest)
  (emit-store-data-value (emit-load-data-value asm-stack src)
                         dest))

(defun emit-copy-stack-value (asm-stack src dest)
  (emit-store-stack-value (emit-load-stack-value asm-stack src)
                         dest))

(defun emit-copy-stack-down (asm-stack src-offset dest-offset count)
  "Copies a COUNT values from the SRC offset to the DEST offset going from down the stack."
  (if (> count 0)
      (emit-copy-stack-down (emit-copy-stack-value asm-stack
                                                   (- src-offset 1)
                                                   (- dest-offset 1))
                            (- src-offset 1)
                            (- dest-offset 1)
                            (- count 1))
      asm-stack))

(defun emit-move-stack (asm-stack src-offset dest-offset count)
  (emit-poppers (emit-copy-stack-down (emit-push (emit-load-stack-value asm-stack src-offset) 0)
                                      (+ 1 src-offset)
                                      (+ 1 dest-offset)
                                      (+ 1 count))
                (- (+ 1 dest-offset) (+ 1 count))))

(defun emit-copy-values-down (asm-stack src dest count &optional (n (- count 1)))
  "Copies a COUNT values from the SRC pointer to the DEST pointer going from the end of the memory region."
  (if (>= n 0)
      (emit-copy-values-down (emit-copy-data-value asm-stack
                                                   (+ src n)
                                                   (+ dest n))
                             src
                             dest
                             count
                             (- n 1))
      asm-stack))

(defun emit-tailcall (asm-stack reg data-offset args callers-bindings)
  ;; pop values, pop caller's values moving stack frame over caller's, call
  (let ((stack-size (/ callers-bindings *REGISTER-SIZE*)))
    (emit-call-jump (emit-move-stack asm-stack args (+ args stack-size) args)
                    reg data-offset)))

(defun emit-init (asm-stack code-segment data-segment toplevel-start toplevel init)
  ;; comments are reverse from how the code is emitted
  ;; perform a reset
  (emit-op
   ;; call init
   (emit-reg-call
    ;; bind init
    (emit-store-data-value
     (emit-integer
      ;; setup DS
      (emit-op
       (emit-integer
        ;; setup CS
        (emit-op asm-stack :load 10 0 15)
        code-segment)
       :load 9 0 15)
      data-segment)
     (env-data-position init toplevel-start toplevel)
     )
    0)
   :reset))
