;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;;
;;; The compiler
;;;

(in-package :repl)

(defvar *CODE-SEGMENT* 0)
(defvar *COMPILER* 0)

#+:repl (require "runtime")
(require "type-sizes")
(require "conditions")
(require "memory")
(require "sequence")
(require "symbol")
(require "string")
(require "compiler/package")
(require "reader")
(require "features")
(require "emitter")
(require "env")

(in-package :repl)

#+:repl (defun repl-compile-inner (package str str-end asm-stack env-start env &optional tail-call return-offset))
#+:repl (defun repl-compile-file (path package str asm-stack env-start env &optional in-require))

(defvar *allow-tail-calls* t)

(defun compile-read-token (package offset)
  (multiple-value-bind (kind value offset token-offset)
      (read-token offset (package-string-segment-offset package))
    (set-package-string-segment-offset package token-offset)
    (values kind value offset)))

(defun compile-scan-list (package offset)
  (multiple-value-bind (new-offset token-offset)
      (scan-list offset (package-string-segment-end package))
    (set-package-string-segment-offset package token-offset)
    new-offset))

(defun gen-func-name (package func-name arity)
  (if (string-position #\/ func-name)
      func-name
      (with-allocation (buffer 256)
        (let ((next-buffer (string-concat func-name "/" buffer)))
          (if (eq arity :any)
              (ptr-write-string "any" (- next-buffer 1))
              (itoa arity (- next-buffer 1)))
          (package-intern package buffer)))))

;;; funcall

(defun compile-funcall-push (package str asm-stack env-start env reg data-offset args)
  (values str
          (emit-funcall asm-stack reg (* *REGISTER-SIZE* data-offset) args)
          (env-pop-bindings env args)))

(defun compile-funcall-tail (package str asm-stack env-start env reg data-offset args return-offset)
  (let ((post-env (env-pop-bindings env args)))
    (values str
            (emit-tailcall asm-stack reg (* *REGISTER-SIZE* data-offset) args (+ (* args *REGISTER-SIZE*) return-offset) (- post-env env-start))
            post-env))
)

;; todo calls need to check if # args matches the arity
;; todo raise an error when an explicit function arity does not match number of arguments

(defun compile-funcall-resolve (package env-start env func-name args)
  "Try to resolve a function name in the lexical bindings and then the toplevel returning the DS or CS register and offset in that segment."
  (let* ((stack-pos (env-stack-position func-name env-start env))
         (func-name-arity (gen-func-name package func-name args))
         ;; (func-name-any (gen-func-name package func-name :any))
         (data-pos (or (env-function-position func-name-arity
                                              (package-symbols-buffer package)
                                              (package-symbols-next-offset package))
                       (env-function-position func-name
                                              (package-symbols-buffer package)
                                              (package-symbols-next-offset package)))))
    (if stack-pos
        (values 11 stack-pos)
        (if data-pos
            (values 9 data-pos)
            nil))))

(defun compile-funcall-it (package str asm-stack env-start env func-name args tail-call return-offset)
  (multiple-value-bind (reg data-offset)
      (compile-funcall-resolve package env-start env func-name args)
    (if (not reg) (error 'undefined-function-error :offset str :name func-name :args args))
    (format *standard-output* ";; Call ~A R~A+~A: ~A args ~A~%" (symbol-string func-name)  reg (* *REGISTER-SIZE* data-offset) args (if tail-call "tail" ""))
    (if (and tail-call (not (= reg 11))) ;; todo need the func's address before tail calling to tail call stack functions
        (compile-funcall-tail package str asm-stack env-start env reg data-offset args return-offset)
        (compile-funcall-push package str asm-stack env-start env reg data-offset args))))

(defun compile-call-argument (package str str-end asm-stack env-start env func-name arg-num tail-call return-offset)
  (format *standard-output* ";; Argument ~A ~A~%" arg-num tail-call)
  ;; compile the current argument
  (multiple-value-bind (offset new-asm-stack new-env token-kind token-value)
      (repl-compile-inner package str str-end asm-stack env-start env)
    ;; make the call when an #\) is read
    (if (and (eq token-kind 'special) (eq token-value (char-code #\))))
        (compile-funcall-it package offset new-asm-stack env-start new-env func-name arg-num tail-call return-offset)
        ;; push result onto stack and move to the next argument
        (compile-call-argument package offset str-end (emit-push new-asm-stack 0) env-start (env-push-binding 0 new-env) func-name (+ 1 arg-num) tail-call return-offset))))

(defun compile-funcall (func-name package str str-end asm-stack env-start env tail-call return-offset)
  (compile-call-argument package str str-end asm-stack env-start env func-name 0 tail-call return-offset))

;;; IF

(defun compile-if-fixer (if-offset then-offset package start-offset asm-stack env-start env)
  (let ((offset-to-else (+ (- then-offset if-offset) *SIZEOF_LONG*))
        (offset-to-end (- asm-stack then-offset)))
    (format *standard-output* ";; IF done ~A ~A ~A ~A ~A ~A~%" start-offset asm-stack if-offset then-offset offset-to-else offset-to-end)
    ;; correct the jump from the CMP to go to ELSE
    (ptr-write-long offset-to-else if-offset)
    ;; correct the jump at the end of THEN past the ELSE
    (ptr-write-long offset-to-end then-offset)
    (values start-offset asm-stack env)))

(defun compile-if-closing (if-offset then-offset package start-offset asm-stack env-start env)
  (format *standard-output* ";; IF closing ~A~%" start-offset)
  ;; read the closing #\)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (compile-if-fixer if-offset then-offset package offset asm-stack env-start env)
        (error 'invalid-token-error :offset start-offset :kind kind :value value))))

(defun compile-if-else (if-offset then-offset package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; compile the ELSE form
  (format *standard-output* ";; IF else ~A~%" tail-call)
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env tail-call return-offset)
    ;; no form, then done
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (compile-if-fixer if-offset then-offset package offset (emit-value asm-stack 'integer 0) env-start env)
        ;; almost done
        (if kind
            (error 'invalid-token-error :offset start-offset :kind kind :value value)
            (compile-if-closing if-offset then-offset package offset asm-stack env-start env)))))

(defun compile-if-then (if-offset package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; compile the THEN form
  (format *standard-output* ";; IF then~%")
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env tail-call return-offset)
    ;; no form, emit zero, then done
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (let ((asm-stack (emit-jump (emit-value asm-stack 'integer 0) #xFFFFFFFF)))
          (compile-if-fixer if-offset
                            (- asm-stack *SIZEOF_LONG*)
                            package
                            offset
                            (emit-value asm-stack 'integer 0)
                            env-start
                            env))
        ;; form so try ELSE
        (if kind
            (error 'invalid-token-error :offset start-offset :kind kind :value value)
            (compile-if-else if-offset
                             (+ asm-stack *SIZEOF_SHORT*)
                             package
                             offset
                             str-end
                             (emit-jump asm-stack #xFFFFFFFF) 
                             env-start
                             env
                             tail-call
                             return-offset)))))

(defun compile-if (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; compile the test
  (format *standard-output* ";; IF condition ~A~%" (if tail-call "tail" ""))
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (if kind (error 'invalid-token-error :offset start-offset :kind kind :value value))
    (compile-if-then (+ (* 2 *SIZEOF_LONG*) *SIZEOF_SHORT* asm-stack)
                     package
                     offset
                     str-end
                     (emit-jump (emit-zero-cmp asm-stack 0 1) #xFFFFFFFF #x1)
                     env-start env tail-call return-offset)))

;;; progn

(defun tail-call? (package offset)
  (multiple-value-bind (call-end)
      (compile-scan-list package offset) ; todo pre-tokenize so this is done once
    (multiple-value-bind (kind value offset)
        (compile-read-token package call-end)
      (and (eq kind 'special) (eq value (char-code #\)))))))

(defun compile-progn (package offset str-end asm-stack env-start env tail-call return-offset &optional (n 0))
  (format *standard-output* ";; expr ~A ~A~%" n (if tail-call "tail" ""))
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package offset str-end asm-stack env-start env (if tail-call (tail-call? package offset)) return-offset)
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (values offset asm-stack env)
        (compile-progn package offset str-end asm-stack env-start env tail-call return-offset (+ 1 n)))))

;;; Forms

(defun compile-body (num-bindings package offset str-end asm-stack env-start env tail-call return-offset)
  (multiple-value-bind (offset asm-stack env kind value)
      (compile-progn package offset str-end asm-stack env-start env tail-call (+ (* num-bindings *REGISTER-SIZE*) (or return-offset 0)))
    (format *standard-output* ";; body closing ~A~%" tail-call)
    (values offset
            (emit-poppers asm-stack num-bindings)
            (env-pop-bindings env num-bindings))))

;;; cond
#+:repl (defun compile-cond-case (package start-offset str-end asm-stack env-start env tail-call return-offset))

(defun compile-cond-case-body (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; jump past body if 0
  (let ((body-start (emit-fake-jump (emit-zero-cmp asm-stack 0 1) #xFFFFFFFF #x1)))
    ;; compile body
    (multiple-value-bind (offset asm-stack env)
        (compile-body 0 package start-offset str-end
                      body-start
                      env-start env tail-call return-offset)
      (let ((body-end (emit-fake-jump asm-stack #xFFFFFFFF)))
        ;; fix the jump over the body
        (emit-fixed-jump body-start (- body-end body-start))
        ;; return now for an any-cond
        ;;(values offset asm-stack env)
        ;; jump to end of cond block, but first compile the other cases
        (multiple-value-bind (offset asm-stack env)
            (compile-cond-case package offset str-end body-end env-start env tail-call return-offset)
          (emit-fixed-jump body-end (- asm-stack body-end))
          (values offset asm-stack env))))))

(defun compile-cond-case-test (package start-offset str-end asm-stack env-start env tail-call return-offset)
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (if kind (error 'invalid-token-error :offset start-offset :kind kind :value value))
    (compile-cond-case-body package offset str-end asm-stack env-start env tail-call return-offset)))

(defun compile-cond-case (package start-offset str-end asm-stack env-start env tail-call return-offset)
  (format *standard-output* ";; cond-case ~A~%" (ptr-read-string start-offset 10))
  ;; compile test expression
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (format *standard-output* ";; cond-case token ~A ~A~%" kind value)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-cond-case-test package offset str-end asm-stack env-start env tail-call return-offset))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values offset asm-stack env))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-cond (package start-offset str-end asm-stack env-start env tail-call return-offset)
  (compile-cond-case package start-offset str-end asm-stack env-start env tail-call return-offset))


;;; LET

(defun compile-let-binding-initializer (num name package start-offset str-end asm-stack env-start env)
  (format *standard-output* ";;  ~A: ~A init~%" num (symbol-string name))
  (multiple-value-bind (offset asm-stack env)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (multiple-value-bind (kind value offset)
        (compile-read-token package offset)
      (if (not (and (eq kind 'special) (eq value (char-code #\)))))
        (error 'malformed-let-error :offset start-offset))
      (values
       offset
       ;; push to stack
       (emit-push asm-stack 0)
       ;; add to env
       (env-push-binding name env)))))

(defun compile-let-binding (num package start-offset str-end asm-stack env-start env)
  ;; read the name
  (multiple-value-bind (kind name offset)
      (compile-read-token package start-offset)
    (if (not (eq kind 'symbol))
      (error 'malformed-let-error :offset start-offset))
    ;; compile the initializer
    (compile-let-binding-initializer num name package offset str-end asm-stack env-start env)))

(defun compile-let-bindings (num package start-offset str-end asm-stack env-start env tail-call return-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ;; read #\(
      ((and (eq kind 'special) (eq value (char-code #\()))
       (multiple-value-bind (offset asm-stack env)
           (compile-let-binding num package offset str-end asm-stack env-start env)
         (compile-let-bindings (+ 1 num) package offset str-end asm-stack env-start env tail-call return-offset))
       )
      ((and (eq kind 'special) (eq value (char-code #\))))
       (format *standard-output* ";; Let body, ~A bindings~%" num)
       (compile-body num package offset str-end asm-stack env-start env tail-call return-offset)
       )
      (t (error 'malformed-let-error :offset start-offset)))
    )
  )

(defun compile-let (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; for each binding, compile and push the value, then push the name's symbol value to env
  ;; compile the body
  ;; clean up the stack
  (format *standard-output* ";; LET~%")
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (if (and (eq kind 'special) (eq value (char-code #\()))
        (compile-let-bindings 0 package offset str-end asm-stack env-start env tail-call return-offset)
        (error 'malformed-let-error :offset start-offset))))

;;; toplevel

(defun compile-toplevel (package start-offset str-end orig-asm-stack env-start env &optional (n 0) starting-code-segment starting-asm-stack)
  (let ((o-code-segment (package-code-segment-offset package)))
    (format *standard-output* ";; toplevel ~A ~A~%" n o-code-segment)
    (multiple-value-bind (offset asm-stack env kind value)
        (repl-compile-inner package start-offset str-end orig-asm-stack env-start env)
      (if (eq kind 'eos)
          ;; copy code to code-segment from asm-stack
          (let* ((asm-stack (emit-return asm-stack))
                 (cs (package-code-segment-offset package))
                 (init-offset (package-code-segment-position package)))
            (package-copy-to-code-segment package
                                          (or starting-asm-stack orig-asm-stack)
                                          (- asm-stack (or starting-asm-stack orig-asm-stack)))
            (format *standard-output* ";;    code segment ~A ~A ~A~%" cs (package-code-segment-position package) *code-segment*)
            (values offset
                    ;; emit the address for toplevel's initializer, offset from CS
                    ;; todo emits a value on each require
                    (emit-value (or starting-asm-stack orig-asm-stack) 'integer init-offset)
                    env))
          (if (eq kind nil)
              (compile-toplevel package offset str-end asm-stack env-start env (+ 1 n) (or starting-code-segment o-code-segment) (or starting-asm-stack orig-asm-stack))
              (error 'invalid-token-error :offset start-offset :kind kind :value value)))))
  )

;;;
;;; Lambda
;;; Lambdas are light weight functions. They can only have a fixed number of arguments and
;;; may have a name for recursion.
;;;

(defun compile-lambda-body (num-bindings package start-offset str-end orig-asm-stack env-start env func-name)
  (multiple-value-bind (offset asm-stack env kind value)
      (compile-progn package
                     start-offset
                     str-end
                     orig-asm-stack
                     env-start
                     (env-push-binding 0 env) ;; account for the return IP
                     *allow-tail-calls*
                     0)
    (format *standard-output* ";; lambda closing ~A~%" (symbol-string func-name))
    ;; (if func-name (setq num-bindings (+ 1 num-bindings)))
    (let* ((asm-stack (emit-return asm-stack num-bindings))
           (cs (package-code-segment-offset package)))
      ;; copy code from asm-stack to code-segment
      (package-copy-to-code-segment package orig-asm-stack (- asm-stack orig-asm-stack))
      (format *standard-output* ";; Copied to code-segment ~A ~A ~A~%" cs (- cs (package-code-segment-buffer package))  (- asm-stack orig-asm-stack))
      (values offset
              orig-asm-stack
              (env-pop-bindings env (- num-bindings 1))))))

(defun compile-lambda-bindings (num package start-offset str-end asm-stack env-start env func-name)
  ;; read symbols and push into env until &optional or &rest or )
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&optional"))
       (error 'malformed-lambda-error :offset start-offset))
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&rest"))
       (error 'malformed-lambda-error :offset start-offset))
      ((eq kind 'symbol)
       (format *standard-output* ";;   ~A: ~A~%" num (symbol-string value))
       (compile-lambda-bindings (+ 1 num) package offset str-end asm-stack env-start (env-push-binding value env) func-name))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (compile-lambda-body num package offset str-end asm-stack env-start env func-name))
      (t (error 'malformed-lambda-error :offset start-offset))))
  )

(defun compile-lambda-arglist (package start-offset str-end orig-asm-stack env-start env name &optional (num-args 0))
  (let ((orig-cs-position (package-code-segment-position package))
        (lambda-env (env-push-binding name env))) ;; account for the lambda's name
    (multiple-value-bind (kind value offset)
        (compile-read-token package start-offset)
      (cond
        ;; start of the arglist
        ((and (eq kind 'special) (eq value (char-code #\()))
         (format *standard-output* ";; Lambda ~A at ~A~%" (symbol-string name) orig-cs-position)
         (multiple-value-bind (offset asm-stack env kind value)
             (compile-lambda-bindings (+ 1 num-args) package offset str-end
                                      ;; push the function pointer to the stack for self reference
                                      (emit-push (emit-value orig-asm-stack 'integer (package-code-segment-position package)) 0)

                                      lambda-env
                                      lambda-env
                                      name)
           (values offset
                   ;; emit the lambda's address in the code-segment onto the
                   ;; asm-stack at the original asm-stack. This will get used
                   ;; when the lambda call returns.
                   (emit-value orig-asm-stack 'integer orig-cs-position)
                   env-start)))
        (t (error 'malformed-lambda-error :offset start-offset))))))

(defun compile-lambda (package start-offset str-end orig-asm-stack env-start env)
  ;; lambdas generate a symbol to name a new function.
  ;; generate a toplevel name
  (multiple-value-bind (name)
      (package-symbol-gen package)
    (multiple-value-bind (kind value offset)
        (compile-read-token package start-offset)
      (format *standard-output* ";; compile-lambda ~A ~A ~A~%" start-offset env-start env)
      (compile-lambda-arglist package
                              (if (and (eq kind 'special)
                                       (eq value (char-code #\()))
                                  start-offset
                                  offset)
                              str-end
                              orig-asm-stack
                              env
                              env
                              (cond
                                ;; named lambda, extract the name and bind it to the stack
                                ((eq kind 'symbol) value)
                                ;; start of the arglist w/o a name
                                ((and (eq kind 'special) (eq value (char-code #\())) name)
                                (t (error 'malformed-lambda-error :offset start-offset)))))))

;; SET

(defun compile-set-local (name stack-pos package start-offset str-end asm-stack env-start env)
  ;; if local, compile and store the value
  (format *standard-output* ";; setting local ~A at SP+~A~%" (symbol-string name) (* *SIZEOF_LONG* stack-pos))
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (if kind (error 'malformed-error :offset start-offset :msg kind))
    ;; eat the terminating )
    (multiple-value-bind (kind value offset)
        (compile-read-token package offset)
      (if (and (eq kind 'special) (eq value (char-code #\))))
          (values offset (emit-store-stack-value asm-stack stack-pos 0) env)
          (error 'malformed-error :offset start-offset :msg "Terminator")))))

(defun compile-set-global (name package start-offset str-end asm-stack env-start env)
  ;; compile the value
  (format *standard-output* ";; setting global ~A ~A ~A~%" (symbol-string name) name (package-symbols-next-offset package))
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (if kind (error 'malformed-error :offset start-offset :msg kind))
    ;; eat the terminating )
    (multiple-value-bind (kind value offset)
        (compile-read-token package offset)
      (if (and (eq kind 'special) (eq value (char-code #\))))
          ;; find the position in the data segment
          (let ((data-pos (package-symbol-offset package name)))
            (if (not data-pos)
                (error 'undefined-variable-error :offset start-offset :name name))
            ;; emit the code to update the value
            (values offset (emit-store-data-value asm-stack data-pos 0) env))
          (error 'malformed-error :offset start-offset :msg "Terminator")))))


(defun compile-define-global (name package start-offset str-end asm-stack env-start env)
  ;; get or define as a global, compile, and set the value
  (format *standard-output* ";; Defining ~A~%" (symbol-string name))
  (package-define package name)
  (compile-set-global name package start-offset str-end asm-stack env-start env))


(defun compile-set (package offset str-end asm-stack env-start env)
  ;; read symbol
  (multiple-value-bind (kind value offset)
      (compile-read-token package offset)
    ;; determine if variable is on the stack or a global
    (let ((stack-pos (env-stack-position value env-start env)))
      (if stack-pos
          (compile-set-local value stack-pos package offset str-end asm-stack env-start env)
          (compile-set-global value package offset str-end asm-stack env-start env)))))

;;; var
(defun compile-var (package offset str-end asm-stack env-start env)
  ;; read symbol
  (multiple-value-bind (kind value offset)
      (compile-read-token package offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-error :offset offset))
    (compile-define-global value package offset str-end asm-stack env-start env)))

;;;
;;; def
;;; Toplevel functions that are like lambdas except they support optional arguments.
;;;

(defun emit-def-arg-initializer (num output asm-stack-start asm-stack-end)
  (format *standard-output* ";; def arg init ~A~%" num)
  (emit-store-stack-value
   (emit-pop
    (emit-store-stack-value
     (emit-load-stack-value
      (emit-push
       (emit-push
        (ptr-copy asm-stack-start
                  output
                  (- asm-stack-end asm-stack-start))))
      2)
     1))
   1))


#+:repl (defun compile-def-optional-binding (num package start-offset str-end asm-stack env-start env func-name))

(defun emit-def-arg-default-initializer (kind value num output)
  (format *standard-output* ";; def arg init value ~A: ~A ~A~%" num kind value)
  (emit-store-stack-value (emit-value (emit-push (emit-load-stack-value output 0) 0)
                                      kind value)
                          1))

(defun compile-def-initializer (name num package offset str-end orig-asm-stack env-start orig-env func-name)
  ;; compiles an argument's initializer and binds it to func-name/num
  (let ((func-name-arity (gen-func-name package func-name num)))
    (package-define package func-name-arity)
    ;; compile the initiailizer
    (multiple-value-bind (offset asm-stack env kind value)
        (repl-compile-inner package offset str-end orig-asm-stack env-start (env-push-binding 0 orig-env))
      ;; eat the )
      (multiple-value-bind (kind value offset)
          (compile-read-token package offset)
        (if (not (and (eq kind 'special)
                      (eq value (char-code #\)))))
            (error 'malformed-error :offset offset))
        ;; copy compiled code to the code-segment
        (let ((new-asm-stack (emit-def-arg-initializer num asm-stack orig-asm-stack asm-stack)))
          (package-copy-to-code-segment package asm-stack (- new-asm-stack asm-stack))
          ;; move to the next argument
          (compile-def-optional-binding (+ 1 num)
                                        package
                                        offset
                                        str-end
                                        ;; set the toplevel binding
                                        (emit-toplevel-store-value orig-asm-stack
                                                                   func-name-arity
                                                                   'integer
                                                                   (package-code-segment-position package)
                                                                   (package-symbols package))
                                        env-start
                                        ;; add argument to env
                                        (env-push-binding name orig-env)
                                        func-name))))))

(defun compile-def-value-initializer (name kind value num package offset str-end orig-asm-stack env-start env func-name)
  ;; emits code to initialize an argument with the default value of 0 and moves to the
  ;; next argument
  (let ((func-name-arity (gen-func-name package func-name num)))
    (let ((asm-stack (emit-def-arg-default-initializer kind value num orig-asm-stack)))
      (package-define package func-name-arity)
      (package-copy-to-code-segment package orig-asm-stack (- asm-stack orig-asm-stack))
      (compile-def-optional-binding (+ 1 num)
                                    package
                                    offset
                                    str-end
                                    ;; set function's toplevel binding
                                    (emit-toplevel-store-value orig-asm-stack func-name-arity 'integer (package-code-segment-position package) (package-symbols package))
                                    env-start
                                    ;; add argument to env
                                    (env-push-binding name env)
                                    func-name))))

(defun compile-def-body (num-bindings package start-offset str-end orig-asm-stack env-start env func-name)
  (let ((cs (package-code-segment-position package)))
    ;; compiles a function's body binding it to func-name/num-bindings
    (let ((func-name-arity (gen-func-name package func-name num-bindings)))
      (package-define package func-name-arity)
      (multiple-value-bind (offset asm-stack env kind value)
          (compile-lambda-body num-bindings
                               package
                               start-offset
                               str-end
                               orig-asm-stack
                               env-start
                               env
                               func-name)
        (values offset
                ;; set toplevel binding
                (emit-toplevel-store-value orig-asm-stack
                                           func-name-arity
                                           'integer
                                           cs
                                           (package-symbols package))
                (env-pop-bindings env num-bindings))))))

(defun compile-def-optional-binding-value (num package offset str-end asm-stack env-start env func-name)
  ;; reads the argument's name before moving to the initializer
  (multiple-value-bind (kind name offset)
      (compile-read-token package offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-error :offset offset))
    (compile-def-initializer name num package offset str-end asm-stack env-start env func-name)))

(defun compile-def-optional-binding (num package start-offset str-end asm-stack env-start env func-name)
  ;; read token, determine symbol or list, and push symbols or list's head into env, compile list's tail and conditionally run before the body
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ;; argument with initializer
      ((and (eq kind 'special)
            (eq value (char-code #\()))
       (compile-def-optional-binding-value num package offset str-end asm-stack env-start env func-name))
      ;; end of arglist
      ((and (eq kind 'special)
            (eq value (char-code #\))))
       ;; don't need def-body to rebind fn/num
       (compile-lambda-body (- num 1) package offset str-end asm-stack env-start env func-name))
      ;; argument without an initializer
      ((eq kind 'symbol)
       (compile-def-value-initializer value 'integer 0 num package offset str-end asm-stack env-start env func-name))
      ;; or error
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-def-optional-bindings (num package start-offset str-end asm-stack env-start env func-name)
  ;; when hitting &optional, define a function named 'name/arity' that points to the first
  ;; initializer, then the next optional argument gets a new function, 'name/arity+1'.
  (let ((name (gen-func-name package func-name num)))
    (package-define package name)
    (compile-def-optional-binding (+ 1 num)
                                  package
                                  start-offset
                                  str-end
                                  (emit-toplevel-store-value asm-stack
                                                             name
                                                             'integer
                                                             (package-code-segment-position package)
                                                             (package-symbols package))
                                  env-start env func-name)))

(defun compile-def-rest-binding (num package start-offset str-end asm-stack env-start env func-name)
  ;; read symbol and push into env, then move to the body
  (error 'not-implemented-error))

(defun compile-def-bindings (num package start-offset str-end asm-stack env-start env func-name)
  ;; read symbols and push into env until &optional or &rest or )
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ;; optional arguments
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&optional"))
       (compile-def-optional-bindings num package offset str-end asm-stack env-start env func-name))
      ;; rest argument
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&rest"))
       (compile-def-rest-binding num package offset str-end asm-stack env-start env func-name))
      ;; argument
      ((eq kind 'symbol)
       (format *standard-output* ";;   ~A: ~A~%" num (symbol-string value))
       (compile-def-bindings (+ 1 num) package offset str-end asm-stack env-start (env-push-binding value env) func-name))
      ;; end of arglist
      ((and (eq kind 'special) (eq value (char-code #\))))
       (compile-def-body num package offset str-end asm-stack env-start env func-name))
      ;; or error
      (t (error 'malformed-lambda-error :offset start-offset)))))

(defun compile-def-arglist (package start-offset str-end orig-asm-stack env-start env name &optional (num-args 0))
  ;; eat the arglist's leading parenthesis
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ;; start of the arglist, start a fresh env
      ((and (eq kind 'special) (eq value (char-code #\()))
       (format *standard-output* ";; def ~A~%" (symbol-string name))
       (compile-def-bindings num-args package offset str-end orig-asm-stack env env name))
      ;; or error
      (t (error 'malformed-lambda-error :offset start-offset)))))


(defun compile-def (package start-offset str-end orig-asm-stack env-start env)
  ;; read the name and compile like a lambda but with special handling for optional arguments
  (multiple-value-bind (kind name offset)
      (compile-read-token package start-offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-error :offset start-offset))
    ; (package-define package name)
    (multiple-value-bind (offset new-asm-stack new-env)
        (compile-def-arglist package offset str-end orig-asm-stack env-start env name)
      (format *standard-output* ";; storing ~A ~A~%" (ptr-read-string name) name)
      (values offset new-asm-stack new-env))))


;;; ISRs

(defun compile-isr-body (package start-offset str-end orig-asm-stack env-start env func-name)
  ;; compiles a function's body binding it to func-name
  (multiple-value-bind (offset asm-stack env kind value)
      (compile-progn package
                     start-offset
                     str-end
                     orig-asm-stack
                     env
                     env
                     nil
                     0)
    (format *standard-output* ";; ISR closing ~A~%" (symbol-string func-name))
    (let* ((asm-stack (emit-isr-return asm-stack))
           (cs (package-code-segment-offset package)))
      ;; copy code from asm-stack to code-segment
      (package-copy-to-code-segment package orig-asm-stack (- asm-stack orig-asm-stack))
      (format *standard-output* ";; Copied to code-segment ~A ~A ~A~%" cs (- cs (package-code-segment-buffer package))  (- asm-stack orig-asm-stack))
      (values offset
              (emit-value orig-asm-stack 'integer (- cs (package-code-segment-buffer package)))
              env-start))))

(defun compile-define-isr (package start-offset str-end orig-asm-stack env-start env)
  ;; read the name and compile like a lambda but without arguments
  (multiple-value-bind (kind name offset)
      (compile-read-token package start-offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-error :offset start-offset))
    ;; define toplevel binding
    (package-define package name)
    (multiple-value-bind (offset asm-stack env kind value)
        (compile-isr-body package offset str-end orig-asm-stack env-start env name)
      (values offset
              ;; set toplevel binding
              (emit-toplevel-store-reg asm-stack
                                       name
                                       (package-symbols package))
              env-start))))


(defun compile-isr (package start-offset str-end orig-asm-stack env-start env)
  ;; lambdas generate a symbol to name a new function.
  ;; generate a toplevel name
  (multiple-value-bind (name)
      (package-symbol-gen package)
    (format *standard-output* ";; compile-isr ~A ~A ~A~%" start-offset env-start env)
    (compile-isr-body package start-offset str-end orig-asm-stack env-start env name)))


;;; quote

(defun compile-quote (package start-offset asm-stack env-start env)
  (multiple-value-bind (kind quoted-value offset)
      (compile-read-token package start-offset)
    (if (eq kind 'symbol)
        (multiple-value-bind (kind value offset)
            (compile-read-token package offset)
          (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
          (if (and (eq kind 'special) (eq value (char-code #\))))
              (values offset (emit-symbol-value asm-stack quoted-value package) env)
              (error 'malformed-error :offset start-offset)))
        (error 'not-implemented-error :feature 'quote :offset start-offset))))

(defun compile-shortcut-quote (package start-offset asm-stack env-start env)
  (multiple-value-bind (kind quoted-value offset)
      (compile-read-token package start-offset)
    (if (eq kind 'symbol)
        (progn
          (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
          (values offset (emit-symbol-value asm-stack quoted-value package) env))
        (error 'malformed-error :offset start-offset))))

;;; asm

#+:repl (defun compile-asm (package start-offset asm-stack env-start env))

(defun compile-asm-op (package start-offset asm-stack env-start env &optional op a b c)
  ;; each op has up to 4 values: op-name a b c
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       (format *standard-output* ";; asm op ~A ~A ~A ~A~%" (symbol-string op) a b c)
       (compile-asm package offset
                    (emit-op asm-stack (symbol-string op) a b c)
                    env-start env))
      ((and (eq kind 'symbol) (eq op nil))
       (compile-asm-op package offset asm-stack env-start env value nil nil nil))
      ((eq kind 'integer)
       (cond
         (c (error 'malformed-error :offset start-offset))
         (b (compile-asm-op package offset asm-stack env-start env op a b value))
         (a (compile-asm-op package offset asm-stack env-start env op a value nil))
         (t (compile-asm-op package offset asm-stack env-start env op value nil nil))
         ))
      (t (error 'malformed-error :offset start-offset)))
    ))

(defun compile-asm (package start-offset asm-stack env-start env)
  ;; each element is an op list or a value
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-asm-op package offset asm-stack env-start env))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values offset asm-stack env))
      ((eq kind 'integer)
       (compile-asm package offset (emit-integer asm-stack value) env-start env))
      ((eq kind 'float)
       (compile-asm package offset (emit-float asm-stack value) env-start env))
      ((eq kind 'symbol)
       (compile-asm package offset (emit-lookup asm-stack value env-start env (package-symbols package)) env-start env))
      (t (error 'malformed-error :offset start-offset)))))

;;; values

(defun compile-values (package start-offset str-end asm-stack env-start env &optional (num 1))
  ;; returns from the caller, keeping arguments in registers
  ;; todo pass register to repl-compile-inner so the mov can be skipped
  ;; todo how to signal that any following forms don't matter? ie: the double ret, if-then's inc? Warn if VALUES is not in a tail position?
  (multiple-value-bind (offset asm-stack env token-kind token-value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (cond
      ((and (eq token-kind 'special) (eq token-value (char-code #\))))
       (format *standard-output* ";; returning~%")
       ;; copy first value into R0 like other returns
       (values offset
               (emit-mov (emit-pop-values asm-stack (- num 1)) 0 1)
               (env-pop-bindings env (- num 1))))
      ((not token-kind)
       (format *standard-output* ";; Return value ~A~%" num)
       ;; push the value and move on
       (compile-values package offset str-end (emit-push asm-stack 0) env-start (env-push-binding 0 env) (+ 1 num)))
      (t (error 'malformed-error :offset start-offset)))))

;;; apply-values

;; todo to get rid of an explicit num-values, turn this into a runtime function/macro?

(defun compile-apply-values-call (num-values offset asm-stack env-start env)
  (format *standard-output* ";; Apply-Values-call~%")
  (values offset (emit-pop (emit-stack-call (emit-mvb-binders asm-stack num-values)
                                            (* (+ 0 num-values) *REGISTER-SIZE*)))
          env))

;; possible refactor: funcall should be equivalent to (apply-values func (values args...))
;; todo apply-values in the tail position should tail call func
(defun compile-apply-values (package start-offset str-end asm-stack env-start env)
  ;; apply-values func num-values expr
  ;; Calls func passing num-values returned with VALUES as argument values.
  (format *standard-output* ";; Apply-Values~%")
  (multiple-value-bind (offset asm-stack env kind value)
      (repl-compile-inner package start-offset str-end asm-stack env-start env)
    (if kind (error 'malformed-error :offset start-offset))
    ;; read num-values
    (multiple-value-bind (kind num-values offset)
        (compile-read-token package offset)
      (if (or (not (eq kind 'integer))
              (<= num-values 0))
          (error 'malformed-error :offset offset))
      ;; read expr
      (multiple-value-bind (offset asm-stack env kind value)
          (repl-compile-inner package offset str-end (emit-push asm-stack 0) env-start env)
        (if kind (error 'malformed-error :offset start-offset))
        ;; eat terminator
        (multiple-value-bind (kind value offset)
            (compile-read-token package offset)
          (if (not (and (eq kind 'special) (eq value (char-code #\)))))
              (error 'invalid-token-error :offset start-offset :kind kind :value value))
          (compile-apply-values-call num-values offset asm-stack env-start env))))))


;;; multiple-value-bind

(defun compile-mvb-bindings (package binding-offset env &optional (num-bindings 0))
  (multiple-value-bind (kind value offset)
      (compile-read-token package binding-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values num-bindings env))
      ((eq kind 'symbol)
       (compile-mvb-bindings package offset (env-push-binding value env) (+ 1 num-bindings)))
      (t (error 'malformed-error :offset binding-offset))))
  )

(defun compile-mvb-expr (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; skip bindings
  (let ((binding-end (compile-scan-list package start-offset)))
    (multiple-value-bind (offset asm-stack env kind)
        ;; compile the expression
        (repl-compile-inner package binding-end str-end asm-stack env-start env)
      (if kind (error 'malformed-error :offset binding-end))
      (multiple-value-bind (num-bindings env)
          ;; push the bindings' values
          (compile-mvb-bindings package start-offset env)
        (format *standard-output* ";; MVB bindings: ~A~%" num-bindings)
        ;; the body
        (compile-body num-bindings package offset str-end (emit-mvb-binders asm-stack num-bindings) env-start env tail-call return-offset))
      )))

(defun compile-mvb (package start-offset str-end asm-stack env-start env tail-call return-offset &optional num-bindings)
  ;; (defmacro multiple-value-bind (bindings expr &rest body)
  ;;   `(apply-values (lambda ,bindings ,@body) ,expr))
  ;; creates a binding in env and pushes the corresponding register
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq num-bindings nil) (eq kind 'special) (eq value (char-code #\()))
       ;; start of binding list
       (compile-mvb-expr package offset str-end asm-stack env-start env tail-call return-offset)
       )
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation-body (var byte-size package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; (with-allocation (binding byte-size) body...)
  (format *standard-output* ";; with-allocation body ~A ~A~%" var byte-size)
  (multiple-value-bind (offset asm-stack env kind value)
      (compile-body 0 package start-offset str-end asm-stack env-start env nil return-offset)
    (values offset (emit-stack-free asm-stack byte-size) (env-pop-alloc byte-size env))))

(defun compile-with-allocation-binding-end (var byte-size package start-offset str-end asm-stack env-start env tail-call return-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (if (not (and (eq kind 'special) (eq value (char-code #\)))))
        (error 'malformed-error :offset start-offset))
    (compile-with-allocation-body var byte-size package offset str-end
                                  (emit-stack-alloc-binding asm-stack byte-size)
                                  env-start
                                  (env-push-alloc-binding var byte-size env)
                                  tail-call
                                  (+ (or return-offset 0) *REGISTER-SIZE* byte-size))))

(defun compile-with-allocation-byte-size (var package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; (with-allocation (binding byte-size) body...)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((eq kind 'integer)
       (format *standard-output* ";; with-allocation ~A ~A~%" var value)
       (compile-with-allocation-binding-end var
                                            (align-bytes value)
                                            package
                                            offset
                                            str-end
                                            asm-stack
                                            env-start
                                            env
                                            tail-call
                                            return-offset))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation-binding (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; (with-allocation (binding byte-size) body...)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((eq kind 'symbol) (compile-with-allocation-byte-size value package offset str-end asm-stack env-start env tail-call return-offset))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation (package start-offset str-end asm-stack env-start env tail-call return-offset)
  ;; (with-allocation (binding byte-size) body...)
  (format *standard-output* ";; with-allocation~%")
  (multiple-value-bind (kind value offset)
      ;; compile the expression
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-with-allocation-binding package offset str-end asm-stack env-start env tail-call return-offset))
      (t (error 'malformed-error :offset start-offset)))))

;;; Require

#-:repl (defvar *load-path* (list (make-pathname :directory '(:relative "."))
                                  (make-pathname :directory '(:relative "." "runtime"))))
#-:repl (defvar *load-extensions* '("nl" "lisp"))

#-:repl
(defun resolve-load-path-ext (path &optional (extensions *load-extensions*))
  (let ((p1 (merge-pathnames (make-pathname :type (first extensions))
                             path)))
    (if (probe-file p1)
        p1
        (if extensions
            (resolve-load-path-ext path (rest extensions))))))

#-:repl
(defun resolve-load-path (path &optional (search-paths *load-path*) (extensions *load-extensions*))
  (let* ((p1 (merge-pathnames (parse-namestring (ptr-read-string path)) (first search-paths)))
         (candidate (resolve-load-path-ext p1 extensions)))
    (if candidate
        (namestring candidate)
        (if search-paths
            (resolve-load-path path (rest search-paths) extensions)
            (error 'no-file-error :path path)))))

#+:repl
(defun resolve-load-path (path &optional search-paths extensions)
  (error 'no-file-error :path path))

(defun compile-require-it (path package start-offset str-end asm-stack env-start env)
  (let* ((full-path (resolve-load-path path))
         (seen (package-required? package full-path)))
    (format *standard-output* ";; Require ~A ~A ~A~%" (ptr-read-string path) full-path seen)
    (if (not seen)
        (progn
          (package-add-source-file package full-path)
          (multiple-value-bind (req-offset asm-stack env kind value)
              (repl-compile-file full-path package str-end asm-stack env-start env *allow-tail-calls*)
            (format *standard-output* ";; ~A required~%" (ptr-read-string path))
            (values start-offset (emit-reg-call asm-stack 0) env)))
        (values start-offset asm-stack env))))

(defun compile-require (package start-offset str-end asm-stack env-start env)
  (multiple-value-bind (kind path offset)
      (compile-read-token package start-offset)
    (if (not (eq kind 'string))
        (error 'malformed-error :offset start-offset))
    (multiple-value-bind (kind value offset)
        (compile-read-token package offset)
      (if (not (and (eq kind 'special)
                    (eq value (char-code #\)))))
          (error 'malformed-error :offset start-offset)
          (compile-require-it path package offset str-end asm-stack env-start env)))))

;;; defmacro

(defun compile-defmacro (start-offset str-end asm-stack env-start env)
  ;; like a function but gets called during compile time, compiles the returned expression,
  ;; and places the resulting expression
  ;; at the call site.
  ;; kind of handles inlined functions
  )


;;; Special forms

(defun special-form? (symbol-offset)
  (let ((sym (symbol-string symbol-offset)))
    (cond
      ((string-equal sym "if") t)
      ((string-equal sym "cond") t)
      ((string-equal sym "let") t)
      ((string-equal sym "let*") t)
      ((string-equal sym "set") t)
      ((string-equal sym "setq") t)
      ((string-equal sym "var") t)
      ((string-equal sym "defvar") t)
      ((string-equal sym "defconstant") t)
      ((string-equal sym "def") t)
      ((string-equal sym "defun") t)
      ((string-equal sym "define-isr") t)
      ((string-equal sym "isr") t)
      ((string-equal sym "quote") t)
      ((string-equal sym "values") t)
      ((string-equal sym "apply-values") t)
      ((string-equal sym "multiple-value-bind") t)
      ((string-equal sym "lambda") t)
      ((string-equal sym "asm") t)
      ((string-equal sym "progn") t)
      ((string-equal sym "with-allocation") t)
      ((string-equal sym "require") t)
      (t nil))))

(defun compile-special-form (form package offset str-end asm-stack env-start env tail-call return-offset)
  (let ((form-str (symbol-string form)))
    (cond
      ((string-equal form-str "if")
       (compile-if package offset str-end asm-stack env-start env tail-call return-offset))
      ((string-equal form-str "cond")
       (compile-cond package offset str-end asm-stack env-start env tail-call return-offset))
      ((or (string-equal form-str "let") (string-equal form-str "let*"))
       (compile-let package offset str-end asm-stack env-start env tail-call return-offset))
      ((or (string-equal form-str "set") (string-equal form-str "setq"))
       (compile-set package offset str-end asm-stack env-start env))
      ((or (string-equal form-str "var") (string-equal form-str "defvar") (string-equal form-str "defconstant"))
       (compile-var package offset str-end asm-stack env-start env))
      ((or (string-equal form-str "def") (string-equal form-str "defun"))
       (compile-def package offset str-end asm-stack env-start env))
      ((string-equal form-str "define-isr")
       (compile-define-isr package offset str-end asm-stack env-start env))
      ((string-equal form-str "isr")
       (compile-isr package offset str-end asm-stack env-start env))
      ((string-equal form-str "quote")
       (compile-quote package offset asm-stack env-start env))
      ((string-equal form-str "values")
       (compile-values package offset str-end asm-stack env-start env))
      ((string-equal form-str "apply-values")
       (compile-apply-values package offset str-end asm-stack env-start env))
      ((string-equal form-str "multiple-value-bind")
       (compile-mvb package offset str-end asm-stack env-start env tail-call return-offset))
      ((string-equal form-str "lambda")
       (compile-lambda package offset str-end asm-stack env-start env))
      ((string-equal form-str "asm")
       (compile-asm package offset asm-stack env-start env))
      ((string-equal form-str "progn")
       (compile-progn package offset str-end asm-stack env-start env tail-call return-offset))
      ((string-equal form-str "with-allocation")
       (compile-with-allocation package offset str-end asm-stack env-start env tail-call return-offset))
      ((string-equal form-str "require")
       (compile-require package offset str-end asm-stack env-start env))
      (t (error 'unknown-special-form-error :offset offset :form form-str)))))

;;; Calls

(defun compile-call (package str str-end asm-stack env-start env tail-call return-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package str)
    (format *standard-output* ";; Calling ~A ~A ~A~%" kind (if (eq kind 'symbol) (symbol-string value) value) (special-form? value))
    (cond
      ;; special forms
      ((and (eq kind 'symbol) (special-form? value))
       (compile-special-form value package offset str-end asm-stack env-start env tail-call return-offset))
      ;; function calls
      ((eq kind 'symbol)
       (compile-funcall value package offset str-end asm-stack env-start env tail-call return-offset))
      (t (error 'malformed-error :offset str))))
  )

;;; Conditional compilation

#+:repl (defun eval-conditional-expr (package start-offset))

(defun eval-conditional-and (package start-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       t)
      ((and (eq kind 'special) (eq value (char-code #\()))
       (eval-conditional-expr package offset))
      ((eq kind 'symbol)
       (if (has-feature? (symbol-string value))
           (eval-conditional-and package offset)
           nil))
      (t (error 'malformed-error :offset start-offset)))))

(defun eval-conditional-or (package start-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       nil)
      ((and (eq kind 'special) (eq value (char-code #\()))
       (eval-conditional-expr package offset))
      ((eq kind 'symbol)
       (if (has-feature? (symbol-string value))
           t
           (eval-conditional-or package offset)))
      (t (error 'malformed-error :offset start-offset)))))

(defun eval-conditional-expr (package start-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "and"))
       (eval-conditional-and package offset))
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "or"))
       (eval-conditional-or package offset))
      ((eq kind 'symbol)
       (has-feature? (symbol-string value)))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-conditional-expr (package start-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package start-offset)
    (cond
      ((eq kind 'symbol)
       (values (has-feature? (symbol-string value)) (symbol-string value) offset))
      ((and (eq kind 'special) (eq value (char-code #\()))
       (let* ((list-end (compile-scan-list package offset))
              (list (ptr-read-string offset (- list-end offset))))
         (format *standard-output* ";; eval: ~A~%" list)
         (values (eval-conditional-expr package offset) list list-end)))
      (t (error 'malformed-error :offset offset)))))

(defun compile-conditional (positive offset package str-end asm-stack env-start env)
  ;; sym is #+ or #-
  ;; followed by an expression evaluated at compile time
  (multiple-value-bind (result test-expr offset)
      (compile-conditional-expr package (+ 1 offset))
    (format *standard-output* ";; Conditional compile: ~A ~A ~A~%" positive result test-expr)
    (repl-compile-inner package
                        (if (or (and positive result) (and (not positive) (not result)))
                            ;; if true the following expression is compiled
                            offset
                            ;; if not it is discarded
                            (progn
                              (format *standard-output* ";; skipping~%")
                              (compile-scan-list package (+ 1 offset))))
                        str-end
                        asm-stack
                        
                        env-start env)))

;;; The actual entry point for the compiler, sorta.

(defun repl-compile-inner (package str str-end asm-stack env-start env &optional tail-call return-offset)
  (multiple-value-bind (kind value offset)
      (compile-read-token package str)
    (cond
      ((eq kind 'integer)
       (values offset (emit-value asm-stack 'integer value) env))
      ((eq kind 'float)
       (values offset (emit-value asm-stack 'float value) env))
      ((eq kind 'string)
       (values offset (emit-string-value asm-stack value package) env))
      ((eq kind 'character)
       (values offset (emit-value asm-stack 'integer value) env))
      ((eq kind 'condition)
       ;; todo move to read-token? messes with tail call detection otherwise
       (compile-conditional value offset package str-end asm-stack env-start env))
      ((and (eq kind 'symbol) (keyword? value))
       (values offset (emit-value asm-stack 'integer value) env))
      ((eq kind 'symbol)
       (values offset (emit-lookup asm-stack value env-start env (package-symbols package)) env))
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-call package offset str-end asm-stack env-start env tail-call return-offset))
      ((and (eq kind 'special) (eq value (char-code #\')))
       (compile-shortcut-quote package offset asm-stack env-start env))
      ((or (eq kind 'special) (eq kind 'eos))
       (values offset asm-stack env kind value))
      (t (error 'invalid-token-error :offset str :kind kind :value value))
      ))
  )

;; todo define __init here and make it the "package" initializer

(defun repl-compile (package str str-end asm-stack env-start env &optional in-require)
  (let ((old-compiler *COMPILER*))
    (if (not in-require)
        (progn
          (setq *COMPILER* package)
          (format *standard-output* ";; Changing *COMPILER* to ~A from ~A~%" *COMPILER* old-compiler)
          ;;(set-compiler-token-segment-data *COMPILER*)
          ;;(setq *CODE-SEGMENT* code-segment)
))
    (multiple-value-bind (req-offset asm-stack env kind value)
        (compile-toplevel package str str-end asm-stack env-start env)
      (setq *COMPILER* old-compiler)
      (values req-offset asm-stack env kind value))))


(defun repl-compile-file (path package str asm-stack env-start env &optional in-require)
  (let ((str-end (ptr-read-file path str)))
    (repl-compile package str str-end asm-stack env-start env in-require)))
