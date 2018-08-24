;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;;
;;; The compiler
;;;

(require "conditions")
(require "memory")
(require "null")
(require "symbol")
(require "sequence")
(require "string")
(require "reader")
(require "type-sizes")
(require "features")
(require "emitter")
(require "env")

(in-package :repl)

(defvar *TOKEN-SEGMENT* 0)
(defvar *CODE-SEGMENT* 0)

;;; funcall

(defun compile-funcall-push (str code-segment asm-stack token-offset env-start env toplevel-start toplevel reg data-offset args)
  (values str
          code-segment
          (emit-funcall asm-stack reg (* *REGISTER-SIZE* data-offset) args)
          token-offset
          (env-pop-bindings env args)
          toplevel))

(defun compile-funcall-tail (str code-segment asm-stack token-offset env-start env toplevel-start toplevel reg data-offset args)
  (let ((post-env (env-pop-bindings env args)))
    (values str
            code-segment
            (emit-tailcall asm-stack reg (* *REGISTER-SIZE* data-offset) args (- post-env env-start))
            token-offset
            post-env
            toplevel)))

(defun compile-funcall-it (str code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name reg data-offset args tail-call)
  (format *standard-output* ";; Call ~A R~A+~A: ~A args ~A~%" (symbol-string func-name)  reg (* *REGISTER-SIZE* data-offset) args (if tail-call "tail" ""))
  (if tail-call
      (compile-funcall-tail str code-segment asm-stack token-offset env-start env toplevel-start toplevel reg data-offset args)
      (compile-funcall-push str code-segment asm-stack token-offset env-start env toplevel-start toplevel reg data-offset args)))

;; todo call to an explicit arity needs to check if # args matches, not mangle symbol
;; lambda needs to define symbols while going through arg list

(defun compile-funcall-resolve (offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name args tail-call)
  (let ((stack-pos (env-stack-position func-name env-start env))
        (data-pos (env-data-position func-name toplevel-start toplevel)))
    (if stack-pos
        (compile-funcall-it offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name 11 stack-pos args tail-call)
        (if data-pos
            (compile-funcall-it offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name 9 data-pos args tail-call)
            (error 'undefined-function-error :offset offset :name (symbol-string func-name))))))

;; todo adjust stack offset in env with each push
(defun compile-call-argument (str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name arg tail-call)
  ;; until an #\) is read
  ;;   call each argument
  (format *standard-output* ";; Argument ~A~%" arg)
  (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env toplevel token-kind token-value)
      (repl-compile-inner str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if (and (eq token-kind 'special) (eq token-value (char-code #\))))
        ;; make the call
        (compile-funcall-resolve offset code-segment new-asm-stack new-token-offset env-start new-env toplevel-start toplevel func-name arg tail-call)
        ;; push result onto stack and move to the next argument
        (compile-call-argument offset str-end code-segment (emit-push new-asm-stack 0) new-token-offset env-start (env-push-binding 0 new-env) toplevel-start toplevel func-name (+ 1 arg) tail-call))))

(defun compile-funcall (func-name str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (compile-call-argument str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name 0 tail-call))

;;; IF

(defun compile-if-fixer (if-offset then-offset start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (let ((offset-to-else (+ (- then-offset if-offset) *SIZEOF_LONG*))
        (offset-to-end (- asm-stack then-offset)))
    (format *standard-output* ";; IF done ~A ~A ~A ~A ~A ~A~%" start-offset asm-stack if-offset then-offset offset-to-else offset-to-end)
    ;; correct the jump from the CMP to go to ELSE
    (ptr-write-long offset-to-else if-offset)
    ;; correct the jump at the end of THEN past the ELSE
    (ptr-write-long offset-to-end then-offset)
    (values start-offset code-segment asm-stack token-offset env toplevel)))

(defun compile-if-closing (if-offset then-offset start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";; IF closing ~A~%" start-offset)
  ;; read the closing #\)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (compile-if-fixer if-offset then-offset offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
        (error 'invalid-token-error :offset start-offset :kind kind :value value))))

(defun compile-if-else (if-offset then-offset start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; compile the ELSE form
  (format *standard-output* ";; IF else~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
    ;; no form, then done
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (compile-if-fixer if-offset nil offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
        ;; almost done
        (if kind
            (error 'invalid-token-error :offset start-offset :kind kind :value value)
            (compile-if-closing if-offset then-offset offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))
    ))
;;;(error 'invalid-token-error :offset start-offset :kind kind :value value)

(defun compile-if-then (if-offset start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; compile the THEN form
  (format *standard-output* ";; IF then~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
    ;; no form, then done
    (if (and (eq kind 'special) (eq kind (char-code #\))))
        (compile-if-fixer if-offset nil offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
        ;; form so try ELSE
        (if kind
            (error 'invalid-token-error :offset start-offset :kind kind :value value)
            (compile-if-else if-offset
                             (+ *SIZEOF_SHORT* asm-stack)
                             offset
                             str-end
                             code-segment
                             (emit-jump asm-stack #xFFFFFFFF) 
                             token-offset
                             env-start
                             env
                             toplevel-start
                             toplevel
                             tail-call)))))

(defun compile-if (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; compile the test
  (format *standard-output* ";; IF condition ~A~%" (if tail-call "tail" ""))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if kind (error 'invalid-token-error :offset start-offset :kind kind :value value))
    (compile-if-then (+ (* 2 *SIZEOF_LONG*) *SIZEOF_SHORT* asm-stack)
                     offset
                     str-end
                     code-segment
                     (emit-jump (emit-zero-cmp asm-stack 0 1) #xFFFFFFFF #x1)
                     token-offset env-start env toplevel-start toplevel tail-call)))

;;; cond

(defun compile-cond-case-body (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; jump past body if 0
  (let ((body-start (emit-fake-jump (emit-zero-cmp asm-stack 0 1) #xFFFFFFFF #x1)))
    ;; compile body
    (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
        (compile-body 0 start-offset str-end code-segment
                      body-start
                      token-offset env-start env toplevel-start toplevel tail-call)
      (let ((body-end (emit-fake-jump asm-stack #xFFFFFFFF)))
        ;; fix the jump over the body
        (emit-fixed-jump body-start (- body-end body-start))
        ;; return now for an any-cond
        ;;(values offset code-segment asm-stack token-offset env toplevel)
        ;; jump to end of cond block, but first compile the other cases
        (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
            (compile-cond-case offset str-end code-segment body-end token-offset env-start env toplevel-start toplevel tail-call)
          (emit-fixed-jump body-end (- asm-stack body-end))
          (values offset code-segment asm-stack token-offset env toplevel)
          )))))

(defun compile-cond-case-test (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if kind (error 'invalid-token-error :offset start-offset :kind kind :value value))
    (compile-cond-case-body offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
    ))

(defun compile-cond-case (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (format *standard-output* ";; cond-case ~A~%" (ptr-read-string start-offset))
  ;; compile test expression
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (format *standard-output* ";; cond-case token ~A ~A~%" kind (ptr-read-string value))
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-cond-case-test offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values offset code-segment asm-stack token-offset env toplevel))
      (t (error 'malformed-error :offset start-offset))
      )))

(defun compile-cond (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (compile-cond-case start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  )

;;; progn

(defun tail-call? (offset token-offset)
  (multiple-value-bind (call-end)
      (scan-list offset token-offset) ; todo pre-tokenize so this is done once
    (multiple-value-bind (kind value offset token-offset)
        (read-token call-end token-offset)
      (and (eq kind 'special) (eq value (char-code #\)))))))

(defun compile-progn (offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call &optional (n 0))
  (format *standard-output* ";; expr ~A ~A~%" n (if tail-call "tail" ""))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel (if tail-call (tail-call? offset token-offset)))
    (if (and (eq kind 'special) (eq value (char-code #\))))
        (values offset code-segment asm-stack token-offset env toplevel)
        (compile-progn offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call (+ 1 n)))))

(defun compile-toplevel (start-offset str-end o-code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel &optional (n 0) starting-code-segment starting-asm-stack)
  (format *standard-output* ";; toplevel ~A ~A~%" n o-code-segment)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end o-code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel)
    (if (and (eq kind 'eos))
        ;; copy code to code-segment from asm-stack
        (let* ((asm-stack (emit-return asm-stack))
               (new-code-segment (ptr-copy (or starting-asm-stack orig-asm-stack)
                                           code-segment
                                           (- asm-stack (or starting-asm-stack orig-asm-stack)))))
          (format *standard-output* ";;    code segment ~A ~A ~A ~A~%" o-code-segment code-segment *code-segment* (- code-segment *code-segment*))
          (values offset
                  
                  new-code-segment
                  ;; emit the address for toplevel's initializer
                  (emit-value (or starting-asm-stack orig-asm-stack) 'integer (- code-segment *code-segment*))
                  token-offset
                  env
                  toplevel))
        (if (eq kind nil)
            (compile-toplevel offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel (+ 1 n) (or starting-code-segment o-code-segment) (or starting-asm-stack orig-asm-stack))
            (error 'invalid-token-error :offset start-offset :kind kind :value value)))))

;;; LET
(defun compile-body (num-bindings offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call &optional (n 0))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (compile-progn offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
    (format *standard-output* ";; body closing~%")
    (values offset
            code-segment
            (emit-poppers asm-stack num-bindings)
            token-offset
            (env-pop-bindings env num-bindings)
            toplevel)
    ))

(defun compile-let-binding-initializer (num name start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";;  ~A: ~A init~%" num (symbol-string name))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (multiple-value-bind (kind value offset token-offset)
        (read-token offset token-offset)
      (unless (and (eq kind 'special) (eq value (char-code #\))))
        (error 'malformed-let-error :offset start-offset))
      (values
       offset
       code-segment
       ;; push to stack
       (emit-push asm-stack 0)
       token-offset
       ;; add to env
       (env-push-binding name env)
       toplevel))))

(defun compile-let-binding (num start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read the name
  (multiple-value-bind (kind name offset token-offset)
      (read-token start-offset token-offset)
    (unless (eq kind 'symbol)
      (error 'malformed-let-error :offset start-offset))
    ;; compile the initializer
    (compile-let-binding-initializer num name offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))

(defun compile-let-bindings (num start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ;; read #\(
      ((and (eq kind 'special) (eq value (char-code #\()))
       (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
           (compile-let-binding num offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
         (compile-let-bindings (+ 1 num) offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
       )
      ((and (eq kind 'special) (eq value (char-code #\))))
       (format *standard-output* ";; Let body, ~A bindings~%" num)
       (compile-body num offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
       )
      (t (error 'malformed-let-error :offset start-offset)))
    )
  )

(defun compile-let (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; for each binding, compile and push the value, then push the name's symbol value to env
  ;; compile the body
  ;; clean up the stack
  (format *standard-output* ";; LET~%")
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (if (and (eq kind 'special) (eq value (char-code #\()))
        (compile-let-bindings 0 offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
        (error 'malformed-let-error :offset start-offset))))


;; Lambda

(defun compile-lambda-optional-bindings (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
;;; when hitting &optional, define a function named 'name/arity' that points to the first
;;; initializer, then the next optional argument gets a new function, 'name/arity+1'.
  ;;; todo where to put the code to set the toplevel bindings?
  ;;; onto the asm-stack, then move to the code-segment before the next binding?
  (multiple-value-bind (name token-offset)
      (gen-func-name func-name num token-offset)
    (compile-lambda-optional-binding num start-offset code-segment asm-stack token-offset env-start env toplevel-start (env-push-binding name toplevel) func-name)
  ))


(defun compile-lambda-optional-binding (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
  ;; read token, determine symbol or list, and push symbols or list's head into env, compile list's tail and conditionally run before the body
  (error 'not-implemented-error)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    ))

(defun compile-lambda-rest-binding (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
  ;; read symbol and push into env, then move to the body
  (error 'not-implemented-error)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    ))

(defun compile-lambda-body (num-bindings start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (compile-progn start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel t)
    (format *standard-output* ";; lambda closing ~A~%" (symbol-string func-name))
    ;; (if func-name (setq num-bindings (+ 1 num-bindings)))
    (values offset
            code-segment
            (emit-return (emit-poppers asm-stack num-bindings))
            token-offset
            (env-pop-bindings env num-bindings)
            toplevel)
    )
  )

(defun compile-lambda-bindings (num start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
  ;; read symbols and push into env until &optional or &rest or )
  ;; todo shift env-start?
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&optional"))
       (compile-lambda-optional-bindings num offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name))
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "&rest"))
       (compile-lambda-rest-binding num offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name))
      ((eq kind 'symbol)
       (format *standard-output* ";;   ~A: ~A~%" num (symbol-string value))
       (compile-lambda-bindings (+ 1 num) offset str-end code-segment (emit-push asm-stack (+ 1 num)) token-offset env-start (env-push-binding value env) toplevel-start toplevel func-name))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (compile-lambda-body num offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name))
      (t (error 'malformed-lambda-error :offset start-offset))))
  )

(defun compile-lambda-arglist (start-offset str-end code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel name &optional (num-args 0))
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ;; start of the arglist
      ((and (eq kind 'special) (eq value (char-code #\()))
       (format *standard-output* ";; Lambda ~A~%" (symbol-string name))
       (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
           (compile-lambda-bindings num-args offset str-end code-segment orig-asm-stack token-offset env env toplevel-start toplevel name)
         (format *standard-output* ";; Copied to code-segment ~A ~A~%" code-segment (- code-segment *code-segment*))
         (values offset
                 ;; copy code from asm-stack to code-segment
                 (ptr-copy orig-asm-stack code-segment (- asm-stack orig-asm-stack))
                 ;; emit the start in the code-segment on the asm-stack at the original asm-stack
                 ;; subtract the CS from the value
                 (emit-value orig-asm-stack 'integer (- code-segment *code-segment*))
                 token-offset
                 env-start
                 toplevel)))
      (t (error 'malformed-lambda-error :offset start-offset)))))

(defun compile-lambda (start-offset str-end code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel)
  ;; lambdas generate a symbol to name a new function.
  ;; todo the value on the stack, and any function pointer is to the /any arity function. That takes an explicit argument number when called.
  ;; generate a toplevel name
  (multiple-value-bind (name token-offset)
      (symbol-gen token-offset)
    (multiple-value-bind (kind value offset token-offset)
        (read-token start-offset token-offset)
      (compile-lambda-arglist (if (and (eq kind 'special)
                                       (eq value (char-code #\()))
                                  start-offset
                                  offset)
                              str-end
                              code-segment
                              ;; need something on the stack, make it the function pointer
                              (emit-push (emit-value orig-asm-stack 'integer (- code-segment *code-segment*)) 0)
                              token-offset
                              env-start
                              (env-push-binding
                               (cond
                                 ;; named lambda, extract the name and bind it to the stack
                                 ((eq kind 'symbol) value)
                                 ;; start of the arglist w/o a name
                                 ((and (eq kind 'special) (eq value (char-code #\())) name)
                                 (t (error 'malformed-lambda-error :offset start-offset)))
                               env)
                              toplevel-start toplevel name 1)
      )  )
)

;; SET

(defun compile-set-local (name stack-pos start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; if local, compile and store the value
  (format *standard-output* ";; setting local ~A at SP+~A~%" (symbol-string name) (* *SIZEOF_LONG* stack-pos))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if kind (error 'malformed-error :offset start-offset :msg kind))
    ;; eat the terminating )
    (multiple-value-bind (kind value offset token-offset)
        (read-token offset token-offset)
      (if (and (eq kind 'special) (eq value (char-code #\))))
          (values offset code-segment (emit-store-stack-value asm-stack stack-pos 0) token-offset env toplevel)
          (error 'malformed-error :offset start-offset :msg "Terminator")))
    ))

(defun compile-set-global (name start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; compile the value
  (format *standard-output* ";; setting global ~A ~A ~A ~A~%" (symbol-string name) name toplevel (env-data-position name toplevel-start toplevel))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if kind (error 'malformed-error :offset start-offset :msg kind))
    ;; eat the terminating )
    (multiple-value-bind (kind value offset token-offset)
        (read-token offset token-offset)
      (if (and (eq kind 'special) (eq value (char-code #\))))
          ;; find the position in the data segment
          (let ((data-pos (env-data-position name toplevel-start toplevel)))
            (if (not data-pos)
                (error 'undefined-variable-error :offset start-offset :name (symbol-string  name)))
            ;; emit the code to update the value
            (values offset code-segment (emit-store-data-value asm-stack data-pos 0) token-offset env toplevel))
          (error 'malformed-error :offset start-offset :msg "Terminator")))))


(defun compile-define-global (name start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; get or define as a global, compile, and set the value
  (let ((toplevel (env-define name toplevel-start toplevel)))
    (compile-set-global name start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))


(defun compile-set (offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read symbol
  (multiple-value-bind (kind value offset token-offset)
      (read-token offset token-offset)
    ;; determine if variable is on the stack or a global
    (let ((stack-pos (env-stack-position value env-start env)))
      (if stack-pos
          (compile-set-local value stack-pos offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
          (compile-set-global value offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))))

;;; var
(defun compile-var (offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read symbol
  (multiple-value-bind (kind value offset token-offset)
      (read-token offset token-offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-statement :offset offset))
    (compile-define-global value offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))

;;; def


(defun compile-def (start-offset str-end code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read the name, compile as an anonymous lambda, and set the binding's value
  (multiple-value-bind (kind name offset token-offset)
      (read-token start-offset token-offset)
    (if (not (eq kind 'symbol))
        (error 'malformed-error :offset start-offset))
    (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env toplevel)
        (compile-lambda-arglist offset str-end code-segment orig-asm-stack token-offset env-start env toplevel-start (env-define name toplevel-start toplevel) name)
      (format *standard-output* ";; storing ~A~%" (ptr-read-string name))
      (values offset code-segment
              ;; lookup and set the value
              (emit-store-data-value  new-asm-stack
                                      (env-data-position name toplevel-start toplevel)
                                      0)
              new-token-offset
              new-env
              toplevel))))


;;; quote

(defun compile-quote (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind quoted-value offset token-offset)
      (read-token start-offset token-offset)
    (if (eq kind 'symbol)
        (multiple-value-bind (kind value offset token-offset)
            (read-token offset token-offset)
          (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
          (if (and (eq kind 'special) (eq value (char-code #\))))
              (values offset code-segment (emit-value asm-stack 'integer quoted-value) token-offset env toplevel)
              (error 'malformed-error :offset start-offset)))
        (error 'not-implemented-error :feature 'quote :offset start-offset))))

(defun compile-shortcut-quote (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind quoted-value offset token-offset)
      (read-token start-offset token-offset)
    (if (eq kind 'symbol)
        (progn
          (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
          (values offset code-segment (emit-value asm-stack 'integer quoted-value) token-offset env toplevel))
        (error 'malformed-error :offset start-offset))))

;;; asm

(defun compile-asm-op (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional op a b c)
  ;; each op has up to 4 values: op-name a b c
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       (compile-asm offset code-segment
                    (emit-op asm-stack (intern (string-upcase (symbol-string op)) "KEYWORD") a b c)
                    token-offset env-start env toplevel-start toplevel))
      ((and (eq kind 'symbol) (eq op nil))
       (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel value nil nil nil))
      ((eq kind 'integer)
       (cond
         (c (error 'malformed-error :offset start-offset))
         (b (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel op a b value))
         (a (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel op a value nil))
         (t (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel op value nil nil))
         ))
      (t (error 'malformed-error :offset start-offset)))
    ))

(defun compile-asm (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; each element is an op list or a value
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values offset code-segment asm-stack token-offset env toplevel))
      ((eq kind 'integer)
       (compile-asm offset code-segment (emit-integer asm-stack value) token-offset env-start env toplevel-start toplevel))
      ((eq kind 'float)
       (compile-asm offset code-segment (emit-float asm-stack value) token-offset env-start env toplevel-start toplevel))
      ((eq kind 'symbol)
       (compile-asm offset code-segment (emit-lookup asm-stack value env-start env toplevel-start toplevel) token-offset env-start env toplevel-start toplevel))
      (t (error 'malformed-error :offset start-offset)))))

;;; values

(defun compile-values (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional (num 1))
  ;; returns from the caller, keeping arguments in registers
  ;; todo pass register to repl-compile-inner to the mov can be skipped
  ;; todo how to signal that any following forms don't matter? ie: the double ret, if-then's inc?
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel token-kind token-value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (cond
      ((and (eq token-kind 'special) (eq token-value (char-code #\))))
       (format *standard-output* ";; returning~%")
       ;; pop values into register
       (values offset code-segment
               (emit-mov (emit-pop-values asm-stack (- num 1)) 0 1)
               token-offset
               (env-pop-bindings env (- num 1))
               toplevel))
      ((not token-kind)
       (format *standard-output* ";; Return value ~A~%" num)
       ;; push the value and move on
       (compile-values offset str-end code-segment (emit-push asm-stack 0) token-offset env-start (env-push-binding 0 env) toplevel-start toplevel (+ 1 num)))
      (t (error 'malformed-error :offset start-offset)))))

;;; apply-values

(defun compile-apply-values-call (offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";; Apply-Values~%")
  (values offset code-segment (emit-poppers (emit-call asm-stack 11 0) 1) token-offset env toplevel))

;; todo refactor: funcall should be equivalent to (apply-values func (values args...))
(defun compile-apply-values (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; apply-values func expr
  ;; Calls func passing any values return with VALUES as argument values.
  (format *standard-output* ";; Apply-Values~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-inner start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (if kind (error 'malformed-error :offset start-offset))
    (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
        (repl-compile-inner offset str-end code-segment (emit-push asm-stack 0) token-offset env-start env toplevel-start toplevel)
      (if kind (error 'malformed-error :offset start-offset))
      ;; eat terminator
      (multiple-value-bind (kind value offset token-offset)
          (read-token offset token-offset)
        (unless (and (eq kind 'special) (eq value (char-code #\)))) (error 'invalid-token-error :offset start-offset :kind kind :value value))
        (compile-apply-values-call offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))))

;;; multiple-value-bind

(defun emit-mvb-binders (asm-stack num-bindings &optional (register 1))
  ;; values places each value in R1 on up. Store in the appropriate stack slot.
  (if (> num-bindings 0)
      (emit-mvb-binders (emit-push asm-stack register) (- num-bindings 1) (+ 1 register))
      asm-stack))

(defun compile-mvb-bindings (binding-offset token-offset env &optional (num-bindings 0))
  (multiple-value-bind (kind value offset token-offset)
      (read-token binding-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       (values num-bindings token-offset env))
      ((eq kind 'symbol)
       (compile-mvb-bindings offset token-offset (env-push-binding value env) (+ 1 num-bindings)))
      (t (error 'malformed-error :offset binding-offset))))
  )

(defun compile-mvb-expr (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; skip bindings
  (let ((binding-end (scan-list start-offset token-offset)))
    (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind)
        ;; compile the expression
        (repl-compile-inner binding-end str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
      (if kind (error 'malformed-error :offset binding-end))
      (multiple-value-bind (num-bindings token-offset env)
          ;; push the bindings' values
          (compile-mvb-bindings start-offset token-offset env)
        (format *standard-output* ";; MVB bindings: ~A~%" num-bindings)
        ;; the body
        (compile-body num-bindings offset str-end code-segment (emit-mvb-binders asm-stack num-bindings) token-offset env-start env toplevel-start toplevel tail-call))
      )))

(defun compile-mvb (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call &optional num-bindings)
  ;; (defmacro multiple-value-bind (bindings expr &rest body)
  ;;   `(apply-values (lambda ,bindings ,@body) ,expr))
  ;; creates a binding in env and pushes the corresponding register
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq num-bindings nil) (eq kind 'special) (eq value (char-code #\()))
       ;; start of binding list
       (compile-mvb-expr offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
       )
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation-body (var byte-size start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; (with-allocation (binding byte-size) body...)
  (format *standard-output* ";; with-allocation body ~A ~A~%" var byte-size)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
      (compile-body 0 start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
    (values offset code-segment (emit-stack-free asm-stack byte-size) token-offset (env-pop-alloc byte-size env) toplevel)))

(defun compile-with-allocation-binding-end (var byte-size start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (if (not (and (eq kind 'special) (eq value (char-code #\)))))
        (error 'malformed-error :offset start-offset))
    (compile-with-allocation-body var byte-size offset str-end code-segment
                                  (emit-stack-alloc-binding asm-stack byte-size)
                                  token-offset
                                  env-start
                                  (env-push-alloc-binding var byte-size env)
                                  toplevel-start
                                  toplevel
                                  tail-call)))

(defun compile-with-allocation-byte-size (var start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; (with-allocation (binding byte-size) body...)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((eq kind 'integer)
       (format *standard-output* ";; with-allocation ~A ~A~%" var value)
       (compile-with-allocation-binding-end var value offset str-end
                                            code-segment
                                            asm-stack
                                            token-offset
                                            env-start
                                            env
                                            toplevel-start
                                            toplevel
                                            tail-call))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation-binding (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; (with-allocation (binding byte-size) body...)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((eq kind 'symbol) (compile-with-allocation-byte-size value offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-with-allocation (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  ;; (with-allocation (binding byte-size) body...)
  (format *standard-output* ";; with-allocation~%")
  (multiple-value-bind (kind value offset token-offset)
      ;; compile the expression
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-with-allocation-binding offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      (t (error 'malformed-error :offset start-offset)))))

;;; Require

(defvar *load-path* ".")

(defun resolve-load-path (path)
  (ptr-read-string path))

(defun compile-require-it (path start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";; Require ~A~%" (ptr-read-string path))
  (multiple-value-bind (req-offset code-segment asm-stack token-offset env toplevel kind value)
      (repl-compile-file (resolve-load-path path) str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
    (format *standard-output* ";; ~A required~%" (ptr-read-string path))
    (values start-offset code-segment asm-stack token-offset env toplevel)))

(defun compile-require (start-offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind path offset token-offset)
      (read-token start-offset token-offset)
    (if (not (eq kind 'string))
        (error 'malformed-error :offset start-offset))
    (multiple-value-bind (kind value offset token-offset)
        (read-token offset token-offset)
      (if (not (and (eq kind 'special)
                    (eq value (char-code #\)))))
          (error 'malformed-error :offset start-offset)
          (compile-require-it path offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))))


;;; Special forms

(defun special-form? (symbol-offset)
  (let ((sym (symbol-string symbol-offset)))
    (or (string-equal sym "if")
        (string-equal sym "cond")
        (string-equal sym "let")
        (string-equal sym "set")
        (string-equal sym "var")
        (string-equal sym "defvar")
        (string-equal sym "def")
        (string-equal sym "defun")
        (string-equal sym "quote")
        (string-equal sym "values")
        (string-equal sym "apply-values")
        (string-equal sym "multiple-value-bind")
        (string-equal sym "lambda")
        (string-equal sym "asm")
        (string-equal sym "progn")
        (string-equal sym "with-allocation")
        (string-equal sym "require"))))

(defun compile-special-form (form offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (let ((form-str (symbol-string form)))
    (cond
      ((string-equal form-str "if")
       (compile-if offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "cond")
       (compile-cond offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "let")
       (compile-let offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "set")
       (compile-set offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((or (string-equal form-str "var") (string-equal form-str "defvar"))
       (compile-var offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((or (string-equal form-str "def") (string-equal form-str "defun"))
       (compile-def offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "quote")
       (compile-quote offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "values")
       (compile-values offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "apply-values")
       (compile-apply-values offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "multiple-value-bind")
       (compile-mvb offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "lambda")
       (compile-lambda offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "asm")
       (compile-asm offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      ((string-equal form-str "progn")
       (compile-progn offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "with-allocation")
       (compile-with-allocation offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ((string-equal form-str "require")
       (compile-require offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel))
      (t (error 'unknown-special-form-error :offset offset :form form-str)))))

;;; Calls

(defun compile-call (str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call)
  (multiple-value-bind (kind value offset token-offset)
      (read-token str token-offset)
    (format *standard-output* ";; Calling ~A ~A ~A~%" kind (if (eq kind 'symbol) (symbol-string value) value) (special-form? value))
    (cond
      ;; special forms
      ((and (eq kind 'symbol) (special-form? value))
       (compile-special-form value offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      ;; function calls
      ((eq kind 'symbol)
       (compile-funcall value offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel tail-call))
      (t (error 'malformed-error :offset str))))
  )

;;; Conditional compilation

(defun eval-conditional-and (start-offset token-offset)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       t)
      ((and (eq kind 'special) (eq value (char-code #\()))
       (eval-conditional-expr offset token-offset))
      ((eq kind 'symbol)
       (if (has-feature? (symbol-string value))
           (eval-conditional-and offset token-offset)
           nil))
      (t (error 'malformed-error :offset start-offset)))))

(defun eval-conditional-or (start-offset token-offset)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'special) (eq value (char-code #\))))
       nil)
      ((and (eq kind 'special) (eq value (char-code #\()))
       (eval-conditional-expr offset token-offset))
      ((eq kind 'symbol)
       (if (has-feature? (symbol-string value))
           t
           (eval-conditional-or offset token-offset)))
      (t (error 'malformed-error :offset start-offset)))))

(defun eval-conditional-expr (start-offset token-offset)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "and"))
       (eval-conditional-and offset token-offset))
      ((and (eq kind 'symbol) (string-equal (symbol-string value) "or"))
       (eval-conditional-or offset token-offset))
      ((eq kind 'symbol)
       (has-feature? (symbol-string value)))
      (t (error 'malformed-error :offset start-offset)))))

(defun compile-conditional-expr (start-offset token-offset)
  (multiple-value-bind (kind value offset token-offset)
      (read-token start-offset token-offset)
    (cond
      ((eq kind 'symbol)
       (values (has-feature? (symbol-string value)) (symbol-string value) offset token-offset))
      ((and (eq kind 'special) (eq value (char-code #\()))
       (let* ((list-end (scan-list offset token-offset))
              (list (ptr-read-string offset (- list-end offset))))
         (format *standard-output* ";; eval: ~A~%" list)
         (values (eval-conditional-expr offset token-offset) list list-end token-offset)))
      (t (error 'malformed-error :offset offset)))))

(defun compile-conditional (positive offset str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; sym is #+ or #-
  ;; followed by an expression evaluated at compile time
  (multiple-value-bind (result test-expr offset token-offset)
      (compile-conditional-expr (+ 1 offset) token-offset)
    (format *standard-output* ";; Conditional compile: ~A ~A ~A~%" positive result test-expr)
    (repl-compile-inner (if (or (and positive result) (and (not positive) (not result)))
                            ;; if true the following expression is compiled
                            offset
                            ;; if not it is discarded
                            (progn
                              (format *standard-output* ";; skipping~%")
                              (scan-list (+ 2 offset) token-offset)))
                        str-end
                        code-segment
                        asm-stack
                        token-offset
                        env-start env
                        toplevel-start toplevel)))

;;; The actual entry point for the compiler, sorta.

(defun repl-compile-inner (str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional tail-call)
  (multiple-value-bind (kind value offset new-token-offset)
      (read-token str token-offset)
    (cond
      ((eq kind 'integer)
       (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env toplevel))
      ((eq kind 'float)
       (values offset code-segment (emit-value asm-stack 'float value) new-token-offset env toplevel))
      ((eq kind 'string)
       (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env toplevel))
      ((eq kind 'character)
       (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env toplevel))
      ((eq kind 'condition)
       ;; todo move to read-token? messes with tail call detection otherwise
       (compile-conditional value offset str-end code-segment asm-stack new-token-offset env-start env toplevel-start toplevel))
      ((and (eq kind 'symbol) (keyword? value))
       (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env toplevel))
      ((eq kind 'symbol)
       (values offset code-segment (emit-lookup asm-stack value env-start env toplevel-start toplevel) new-token-offset env toplevel))
      ((and (eq kind 'special) (eq value (char-code #\()))
       (compile-call offset str-end code-segment asm-stack new-token-offset env-start env toplevel-start toplevel tail-call))
      ((and (eq kind 'special) (eq value (char-code #\')))
       (compile-shortcut-quote offset code-segment asm-stack new-token-offset env-start env toplevel-start toplevel))
      ((or (eq kind 'special) (eq kind 'eos))
       (values offset code-segment asm-stack new-token-offset env toplevel kind value))
      (t (error 'invalid-token-error :offset str :kind kind :value value))
      ))
  )

(defun repl-compile (str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (setf *TOKEN-SEGMENT* token-offset)
  (setf *CODE-SEGMENT* code-segment)
  (compile-toplevel str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  )

(defun repl-compile-file (path str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (let ((str-end (ptr-read-file path str)))
    (repl-compile str str-end code-segment asm-stack token-offset env-start env toplevel-start toplevel)))