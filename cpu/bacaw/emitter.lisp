;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "symbol")
(require "string")
(require "type-sizes")
(require "logging")

(in-package :repl)

(require "runtime/bc/cpu")

(defun make-short (&optional a b c d)
  (logior (or a 0)
          (ash (or b 0) 4)
          (ash (or c 0) 8)
          (ash (or d 0) 12)))

(defun make-op (op &optional a b c)
  (cond
    ((eq op :NOP) (make-short 0 0 a b))
    ((eq op :NOT) (make-short 0 1 a b))
    ((eq op :OR) (make-short 0 2 a b))
    ((eq op :XOR) (make-short 0 3 a b))
    ((eq op :AND) (make-short 0 4 a b))
    ((eq op :BSL) (make-short 0 5 a b))
    ((eq op :INT) (make-short 0 7 a b))
    ((eq op :HALT) (make-short 0 8 a b))
    ((eq op :NEG) (make-short 0 9 a b))
    ((eq op :RTI) (make-short 0 12 a b))
    ((eq op :BSR) (make-short 0 13 a b))
    ((eq op :CLS) (make-short 0 #xE a b))
    ((eq op :INTR) (make-short 0 #xF a b))
    ((eq op :INC) (make-short #x1 a b c))
    ;; signed
    ((eq op :CMPI) (make-short #x2 0 a b))
    ((eq op :ADDI) (make-short #x2 #x1 a b))
    ((eq op :MODI) (make-short #x2 #x8 a b))
    ((eq op :SUBI) (make-short #x2 #x9 a b))
    ((eq op :MULI) (make-short #x2 #x2 a b))
    ((eq op :POWI) (make-short #x2 #x4 a b))
    ((eq op :DIVI) (make-short #x2 #xa a b))
    ((eq op :CONVI) (make-short #x2 #xb a b))
    ((eq op :ROOTI) (make-short #x2 #xc a b))
    ((eq op :LOGI) (make-short #x2 #xd a b))
    ((eq op :CMP) (make-short #x2 0 a b))
    ;; unsigned
    ((eq op :CMPU) (make-short #x3 0 a b))
    ((eq op :ADDU) (make-short #x3 #x1 a b))
    ((eq op :MODU) (make-short #x3 #x8 a b))
    ((eq op :SUBU) (make-short #x3 #x9 a b))
    ((eq op :MULU) (make-short #x3 #x2 a b))
    ((eq op :POWU) (make-short #x3 #x4 a b))
    ((eq op :DIVU) (make-short #x3 #xa a b))
    ((eq op :CONVU) (make-short #x3 #xb a b))
    ((eq op :ROOTU) (make-short #x3 #xc a b))
    ((eq op :LOGU) (make-short #x3 #xd a b))
    ;; float
    ((eq op :CMPF) (make-short #x4 0 a b))
    ((eq op :ADDF) (make-short #x4 #x1 a b))
    ((eq op :MODF) (make-short #x4 #x8 a b))
    ((eq op :SUBF) (make-short #x4 #x9 a b))
    ((eq op :MULF) (make-short #x4 #x2 a b))
    ((eq op :POWF) (make-short #x4 #x4 a b))
    ((eq op :CEILF) (make-short #x4 #x6 a b))
    ((eq op :ROUNDF) (make-short #x4 #x7 a b))
    ((eq op :DIVF) (make-short #x4 #xa a b))
    ((eq op :CONVF) (make-short #x4 #xb a b))
    ((eq op :ROOTF) (make-short #x4 #xc a b))
    ((eq op :LOGF) (make-short #x4 #xd a b))
    ((eq op :FLOORF) (make-short #x4 #xe a b))

    ((eq op :LOAD) (make-short #x5 a b c))
    ((eq op :POP) (make-short #x6 a b c))
    ((eq op :CIE) (make-short #x7 #x0 a b))
    ((eq op :CALL) (make-short #x7 #x7 a b))
    ((eq op :SIE) (make-short #x7 #x8 a b))
    ((eq op :SLEEP) (make-short #x7 #x9 a b))
    ((eq op :RET) (make-short #x7 #xc a b))
    ((eq op :RESET) (make-short #x7 #x1 a b))
    ((eq op :CALLR) (make-short #x7 #xF a b))
    ((eq op :MOV) (make-short #x8 a b c))
    ((eq op :DEC) (make-short #x9 a b c))
    ((eq op :STORE) (make-short #xD a b c))
    ((eq op :PUSH) (make-short #xE a b c))
    (t (error 'unknown-op-error :op op))))

(defun emit-op (stack op &optional a b c)
  (let ((op (intern-keyword op)))
    (logger :debug "~A ~A ~A ~A    ~A~%" op (or a 0) (or b 0) (or c 0) (make-op op a b c))
    (ptr-write-short (make-op op a b c) stack)
    (+ stack *SIZEOF_SHORT*)))

(defun emit-float (stack value)
  (logger :debug "float32(~A)~%" value)
  (ptr-write-float value stack)
  (+ stack *SIZEOF_FLOAT*)
  )

(defun emit-integer (stack value)
  (logger :debug "ulong(~A)~%" value)
  (ptr-write-long value stack)
  (+ stack *SIZEOF_LONG*)
  )

(defun emit-value (asm-stack kind value &optional (reg 0) (condition 0))
  (logger :debug ";; Value ~A ~A -> ~A~%" kind value reg)
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
  (logger :debug ";; store R~A at R9+~A~%" register offset)
  (emit-integer (emit-op asm-stack :store register 0 9) (* *REGISTER-SIZE* offset))
  )

(defun emit-lookup-call (asm-stack symbol symbol-index)
  ;; load the symbol value into R1 and the toplevel lookup from the top of the stack
  ;; todo need to lookup the lookup function since lambda's set env to env-start
  (logger :debug ";;  globally~%")
  (emit-integer (emit-op (emit-integer (emit-op (emit-integer (emit-op asm-stack 'load 1 0 15)
                                                              symbol)
                                                :load 0 0 11)
                                       (symbol-index-position symbol-index))
                         :call 0 10)
                0))

(defun emit-lookup-global (asm-stack symbol symbol-index)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (logger :debug ";; Lookup global ~A ~S ~A~%" symbol (symbol-string symbol) symbol-index)
  (let ((stack-pos (symbol-index-offset symbol-index symbol)))
    (if stack-pos
        (emit-load-data-value asm-stack stack-pos))))

(defun emit-lookup-local (asm-stack symbol env-start env)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (logger :debug ";; Lookup local ~A ~S ~A ~A ~A~%" symbol (symbol-string symbol) env-start env (symbol-id symbol))
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
        (error 'undefined-variable-error :name symbol))))

(defun emit-lookup-or-nil (asm-stack symbol env-start env toplevel)
  (let ((new-asm (emit-lookup-inner asm-stack symbol env-start env toplevel)))
    (if new-asm
        new-asm
        (emit-value asm-stack 'integer 0))))

(defun emit-string-segment-position (asm-stack package)
  (let* ((string-segment-sym (package-intern package "*STRING-SEGMENT*"))
         (asm-stack (emit-lookup-global asm-stack string-segment-sym (package-symbols package))))
    (if asm-stack
        asm-stack
        (error 'undefined-variable-error :name string-segment-sym))))

(defun emit-string-value (asm-stack ptr package)
  ;; need to emit code to adjust the string's offset into the string segment
  (emit-op
   (emit-op (emit-string-segment-position
             (emit-value asm-stack 'integer (- ptr (package-string-segment-data package)) 1)
             package)
            :cls)
   :addi 1 14))

(defun emit-symbol-value (asm-stack symbol package)
  (emit-string-value asm-stack symbol package))

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
  (logger :debug ";; ~A poppers~%" num-bindings)
  (if (> num-bindings 0)
      (emit-integer (emit-op asm-stack :inc 11)
                    (* num-bindings *REGISTER-SIZE*))
      asm-stack))

(defun emit-mov (asm-stack dest src)
  (emit-op asm-stack :mov dest src))

(defun emit-stack-alloc (asm-stack size)
  (logger :debug ";; stack-alloc ~A bytes~%" size)
  ;; shift SP
  (emit-integer (emit-op asm-stack :dec 11)
                (align-bytes size)))

(defun emit-stack-alloc-value (asm-stack size)
  (logger :debug ";; stack-alloc-value ~A bytes~%" size)
  ;; shift SP, mov SP to return value
  (emit-mov (emit-stack-alloc asm-stack size)
            0 11))

(defun emit-stack-alloc-binding (asm-stack size)
  (logger :debug ";; stack-alloc-binding ~A bytes~%" size)
  ;; shift SP, push SP
  (emit-push (emit-stack-alloc asm-stack size)
             11))

(defun emit-stack-free (asm-stack size)
  (logger :debug ";; Freeing ~A bytes~%" (+ (align-bytes size) (* 1 *REGISTER-SIZE*)))
  (emit-integer (emit-op asm-stack :inc 11) (+ (align-bytes size) (* 1 *REGISTER-SIZE*))))

(defun emit-jump (asm-stack offset &optional (condition 0))
  (emit-integer (emit-op asm-stack :inc 12 condition) offset))

(defun emit-fake-jump (asm-stack offset &optional (condition 0))
  (emit-jump asm-stack offset condition))

(defun emit-fixed-jump (asm-stack new-jump-offset)
  (logger :debug ";; fixing body jump ~A ~A~%" (+ new-jump-offset *SIZEOF_LONG*) (- asm-stack *SIZEOF_LONG*))
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

(defun emit-save-context (asm-stack &optional (reg 0))
  (if (< reg *REGISTER-SP*)
      (emit-save-context (if (system-register? reg)
                             asm-stack
                             (emit-push asm-stack reg))
                         (+ 1 reg))
      asm-stack))

(defun emit-restore-context (asm-stack &optional (reg *REGISTER-SP*))
  (if (> reg 0)
      (let ((next-reg (- reg 1)))
        (emit-restore-context (emit-pop asm-stack next-reg) next-reg))
      asm-stack))

(defun emit-isr-prolog (asm-stack)
  (emit-save-context asm-stack))

(defun emit-isr-return (asm-stack)
  (emit-op (emit-restore-context asm-stack) :rti))

#+:never
(defun emit-reg-call (asm-stack reg)
  (emit-integer (emit-op (emit-op (emit-op asm-stack :cls #x7)
                                  :addi 10 14)
                         :call 0 reg)
                0))

(defun emit-reg-call (asm-stack reg)
  (emit-op asm-stack :callr 10 reg))

(defun emit-reg-jump (asm-stack reg)
  (logger :debug ";; reg-jump R~A~%" reg)
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
  (if (>= count 0)
      (emit-copy-stack-down (emit-copy-stack-value asm-stack src-offset dest-offset)
                            (- src-offset 1)
                            (- dest-offset 1)
                            (- count 1))
      asm-stack))

(defun emit-move-stack (asm-stack return-offset src-offset dest-offset count)
  ;; update SP
  (emit-poppers
   ;; move the arguments up the stack
   (emit-copy-stack-down
    ;; save the return address
    (emit-push (emit-load-stack-value asm-stack return-offset) 0)
    src-offset dest-offset count)
   (-  dest-offset src-offset)))

(defun emit-tailcall (asm-stack reg data-offset num-args return-offset callers-bindings)
  ;; Copies the call's arguments and the return address at the return-offset up the stack.
  ;; The address at the return-offset is in bytes. Callers-bindings are in number of bindings.
  ;; todo return-offset is in bytes: everything else is number of bindings
  (let ((stack-size (/ callers-bindings *REGISTER-SIZE*)))
    (logger :debug ";; tailcall R~A+~A, ~A args, return at ~A, ~A bindings~%" reg data-offset num-args return-offset stack-size)
    ;; pop values, pop caller's values moving stack frame over caller's, call
    (emit-call-jump (emit-move-stack asm-stack (/ return-offset *REGISTER-SIZE*) num-args (+ num-args stack-size) num-args)
                    reg data-offset)))

(defun emit-mvb-binders (asm-stack num-bindings &optional (register 1))
  ;; values places each value in R1 on up. Store in the appropriate stack slot.
  (if (> num-bindings 0)
      (emit-mvb-binders (emit-push asm-stack register) (- num-bindings 1) (+ 1 register))
      asm-stack))

(defun emit-init (asm-stack code-segment data-segment toplevel-start toplevel init init-sym string-segment-sym)
  ;; comments are reverse from how the code is emitted
  (let ((string-segment-jump (emit-integer
                              (emit-op
                               ;; bind init
                               (emit-store-data-value
                                ;; setup DS
                                (emit-integer
                                 (emit-op
                                  ;; setup CS
                                  (emit-integer
                                   (emit-op asm-stack :load 10 0 15)
                                   code-segment)
                                  :load 9 0 15)
                                 data-segment)
                                (env-data-position init-sym toplevel-start toplevel)
                                )
                               :load 1 0 15)
                              #xFFFFFFFF)))
    (values
     ;; perform a reset
     (emit-op
      ;; call init
      (emit-reg-call
       ;; bind *string-segment* with a temporary value
       (emit-store-data-value
        string-segment-jump
        (env-data-position string-segment-sym toplevel-start toplevel)
        1)    
       0)
      :halt)
     (- string-segment-jump *SIZEOF_LONG*))))

