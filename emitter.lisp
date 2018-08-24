;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")

(in-package :repl)

(defun make-short (&optional a b c d)
  (logior (or a 0)
          (ash (or b 0) 4)
          (ash (or c 0) 8)
          (ash (or d 0) 12)))

(defun make-op (op &optional a b c)
  (case op
    (:NOP (make-short 0 0 0 0))
    (:LOAD (make-short #x5 a b c))
    (:STORE (make-short #xD a b c))
    (:CALL (make-short #x7 #x7 a b))
    (:RET (make-short #x7 #xc a b))
    (:PUSH (make-short #xE a b c))
    (:POP (make-short #x6 a b c))
    (:INC (make-short #x1 a b c))
    (:ADDI (make-short #x2 #x1 a b))
    (:SUBI (make-short #x2 #x9 a b))
    (:MULI (make-short #x2 #x2 a b))
    (:DIVI (make-short #x2 #xa a b))
    (:CLS (make-short 0 #xe a b))
    (:DEC (make-short #x9 a b c))
    (:MOV (make-short #x8 a b c))
    (:CMP (make-short #x2 0 a b))
    (:RESET (make-short #x7 #x1 a b))
    (:HALT (make-short 0 8 a b))
    (t (error 'unknown-op-error :op op))))

(defun emit-op (stack op &optional a b c)
  (format *standard-output* "~A ~A ~A ~A    ~A~%" op (or a 0) (or b 0) (or c 0) (make-op op a b c))
  (ptr-write-short (make-op op a b c) stack)
  (+ stack *SIZEOF_SHORT*))

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

(defun emit-value (asm-stack kind value)
  (format *standard-output* ";; Value ~A ~A~%" kind value)
  (if (eq kind 'float)
      (emit-float (emit-op asm-stack :load 0 0 15) value)
      (emit-integer (emit-op asm-stack :load 0 0 15) value)))

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

(defun emit-lookup-call (asm-stack symbol env-start env)
  ;; load the symbol value into R1 and the toplevel lookup from the top of the stack
  ;; todo need to lookup the lookup function since lambad's set env to env-start
  (format *standard-output* ";;  globally~%")
  (emit-integer (emit-op (emit-integer (emit-op (emit-integer (emit-op asm-stack 'load 1 0 15)
                                                              symbol)
                                                :load 0 0 11)
                                       (- env env-start))
                         :call 0 10)
                0))

(defun emit-lookup-global (asm-stack symbol env-start env)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (format *standard-output* ";; Lookup global ~A ~A~%" (symbol-string symbol) env)
  (let ((stack-pos (env-data-position symbol env-start env)))
    (if stack-pos
        (emit-load-data-value asm-stack stack-pos))))

(defun emit-lookup-local (asm-stack symbol env-start env)
  ;; search env for symbol
  ;; if found, get its position and emit code to load its value
  (format *standard-output* ";; Lookup local ~A ~A~%" (symbol-string symbol) env)
  (let ((stack-pos (env-stack-position symbol env-start env)))
    (if stack-pos
        (emit-load-stack-value asm-stack stack-pos))))

(defun emit-lookup-inner (asm-stack symbol env-start env toplevel-start toplevel)
  (let ((new-asm (emit-lookup-local asm-stack symbol env-start env)))
    (if new-asm
        new-asm
        (emit-lookup-global asm-stack symbol toplevel-start toplevel))))

(defun emit-lookup (asm-stack symbol env-start env toplevel-start toplevel)
  (let ((new-asm (emit-lookup-inner asm-stack symbol env-start env toplevel-start toplevel)))
    (if new-asm
        new-asm
        (error 'undefined-variable-error :name (symbol-string symbol)))))

(defun emit-lookup-or-nil (asm-stack symbol env-start env toplevel-start toplevel)
  (let ((new-asm (emit-lookup-inner asm-stack symbol env-start env toplevel-start toplevel)))
    (if new-asm
        new-asm
        (emit-value asm-stack 'integer 0))))

(defun emit-push (asm-stack src)
  (emit-op asm-stack :push src))

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

(defun emit-mov (asm-stack dest src)
  (emit-op asm-stack :mov dest src))

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

(defun emit-return (asm-stack)
  (emit-op asm-stack :ret))

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
  (if (>= num 8) (error 'argument-error :number num)
      (if (> num 0)
          (emit-pop-values (emit-op asm-stack :pop num) (- num 1))
          asm-stack)))

(defun emit-funcall (asm-stack reg data-offset args)
  (emit-call (emit-pop-values asm-stack args) reg data-offset))

(defun emit-tailcall (asm-stack reg data-offset args callers-bindings)
  ;; pop values, pop caller's values moving stack frame over caller's, call
  (emit-call-jump (emit-op (emit-poppers (emit-pop-values asm-stack args)
                                         (/ callers-bindings *REGISTER-SIZE*))
                           :nop) 
                  reg data-offset))

(defun emit-init (asm-stack code-segment data-segment toplevel-start toplevel init)
  (emit-op
   (emit-reg-call
    (emit-store-data-value
     (emit-integer (emit-op
                    (emit-integer (emit-op asm-stack :load 10 0 15)
                                  code-segment)
                    :load 9 0 15)
                   data-segment)
     (env-data-position init toplevel-start toplevel))
    0)
   :reset))
