(defpackage :repl
  (:use :cl)
  (:export :undefined-variable-error
           :unknown-op-error
           :malformed-error
           :malformed-let-error
           :env-symbol-position
           :env-push-binding
           :env-pop-bindings
           :read-number
           :read-symbol
           :read-token
           :*TOKEN* :*MEMORY*
           :ptr-read-byte
           :ptr-write-byte
           :ptr-read-array
           :ptr-read-short
           :ptr-write-short
           :ptr-read-long
           :ptr-write-long
           :ptr-read-float
           :ptr-write-float
           :ptr-write-string
           :ptr-read-string
           :ptr-find-string
           :symbol-string
           :space?
           :null?
           :symbol-char?
           :special?
           :alpha?
           :digit?
           :index-of
           :test
           :test-read-token
           :test-compile
           :test-compile-let
           :test-compile-quote
           :test-compile-set-local
           :test-compile-set-global
           :test-compile-lambda
           :test-compile-named-lambda
           :test-compile-set-lambda
           :string=
           :repl-read
           :make-op
           :make-short
           :emit-lookup
           :emit-lookup-call
           :emit-value
           :repl-compile
           ))

(in-package :repl)

(define-condition repl-error ()
  ((offset :initarg :offset :initform nil)
   (msg :initarg :msg :initform nil))
  (:report (lambda (condition stream)
             (format stream "Error at offset ~A~%" (slot-value condition 'offset))
             (if (slot-value condition 'msg)
                 (format stream msg)))))

(define-condition unknown-op-error (repl-error)
  ((op :initarg :op :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown op error: ~A~%" (slot-value condition 'op)))))

(define-condition undefined-variable-error (repl-error)
  ((variable :initarg :variable :initform nil)
   (offset :initarg :offset :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "Undefined symbol at ~A: ~A ~A~%"
                     (slot-value condition 'offset)
                     (symbol-string (slot-value condition 'variable))
                     (slot-value condition 'variable)))))

(define-condition not-implemented-error (repl-error)
  ((feature :initarg :feature :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A not implemented at ~A~%"
                     (slot-value condition 'feature)
                     (slot-value condition 'offset)))))

(define-condition unknown-special-form-error (repl-error)
  ((form :initarg :form :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown special form at ~A: ~A~%"
                     (slot-value condition 'offset)
                     (slot-value condition 'form)))))

(define-condition malformed-error (repl-error)
  ((form :initarg :form :initform nil))
  (:report (lambda (condition stream)
             (format stream "Malformed form at ~A: ~A~%"
                     (slot-value condition 'offset)
                     (or (slot-value condition 'form) (ptr-read-string (slot-value condition 'offset)))))))

(define-condition malformed-let-error (malformed-error) ())
(define-condition malformed-lambda-error (malformed-error) ())

(define-condition invalid-token-error (malformed-error)
  ((kind :initarg :kind :initform nil)
   (value :initarg :value :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid token at ~A: ~A ~A~%"
                     (slot-value condition 'offset)
                     (slot-value condition 'kind)
                     (slot-value condition 'value)))))

;;;
;;; Compatibility layer functions
;;;

;;; All input comes in through *MEMORY* and any array values like symbols
;;; get written to *MEMORY* as well.
(defvar *MEMORY* (make-array (* 8 1024)))
(defvar *TOKEN-SEGMENT* 0)
;;; Base of the read numbers
(defvar *NUMBER-BASE* 10)

(defun ptr-read-byte (ptr)
  (aref *MEMORY* ptr))

(defun ptr-write-byte (c ptr)
  (setf (aref *MEMORY* ptr) c))

(defun ptr-read-array (ptr elements &optional (arr (make-array elements)))
  (dotimes (n elements)
    (setf (aref arr n) (ptr-read-byte (+ ptr n))))
    arr)

(defun ptr-copy (src dest count)
  (if (> count *SIZEOF_LONG*)
      (progn
        (ptr-write-long (ptr-read-long src) dest)
        (ptr-copy (+ src *SIZEOF_LONG*) (+ dest *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
    (if (> count 0)
        (progn
          (ptr-write-byte (ptr-read-byte src) dest)
          (ptr-copy (+ src 1) (+ dest 1) (- count 1)))
      dest)))

(defun ptr-read-short (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)))

(defun ptr-write-short (n ptr)
  (ptr-write-byte (ldb (byte 8 0) n) ptr)
  (ptr-write-byte (ldb (byte 8 8) n) (+ 1 ptr)))

(defun ptr-read-long (ptr)
  (logior (ptr-read-byte ptr)
          (ash (ptr-read-byte (+ ptr 1)) 8)
          (ash (ptr-read-byte (+ ptr 2)) 16)
          (ash (ptr-read-byte (+ ptr 3)) 24)))

(defun ptr-write-long (n ptr)
  (ptr-write-byte (ldb (byte 8 0) n) ptr)
  (ptr-write-byte (ldb (byte 8 8) n) (+ 1 ptr))
  (ptr-write-byte (ldb (byte 8 16) n) (+ 2 ptr))
  (ptr-write-byte (ldb (byte 8 24) n) (+ 3 ptr))
  )

(defun ptr-write-string (str ptr)
  (dotimes (n (length str))
    (ptr-write-byte (char-code (aref str n)) (+ n ptr)))
  (ptr-write-byte 0 (+ (length str) ptr)))

(defun null? (c)
  (eq c 0))

#+:sbcl
(defun ptr-read-string (ptr &optional acc)
  (let* ((c (ptr-read-byte ptr)))
    (if (null? c)
        acc
      (progn
        (setf acc (concatenate 'string acc (list (code-char c))))
        (ptr-read-string (+ 1 ptr) acc)))))

#+:sbcl
(defun ptr-read-float (ptr)
  (let ((bits (ptr-read-long ptr)))
    (sb-kernel:make-single-float bits)))

#+:sbcl
(defun ptr-write-float (value ptr)
  (ptr-write-long (sb-kernel:single-float-bits value) ptr))

(defun ptr-find-string (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (> (length current) 0)
            (if (string= current str)
                stack-start
              (ptr-find-string str (+ 1 stack-start (length current)) stack-end))))))

#-:sbcl
(defun pointer-of (needle haystack)
  (let ((h (ptr-read-byte haystack)))
    (cond
     ((null? h) nil)
     ((eq needle h) haystack)
     (t (pointer-of needle (+ 1 haystack))))))

#-:sbcl
(defun index-of (needle haystack)
  (let ((ptr (pointer-of needle haystack)))
    (if ptr
        (- ptr needle)
      nil)))

#+:sbcl
(defun index-of (needle haystack &optional (n 0))
  (when (< n (length haystack))
    (if (eq needle (char-code (aref haystack n)))
        n
      (index-of needle haystack (+ 1 n)))))

(defun symbol-id (symbol-offset)
  (ptr-find-string (ptr-read-string symbol-offset) *TOKEN-SEGMENT* symbol-offset))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string symbol-offset)))

#-:sbcl
(defun string= (a b &optional (as 0) (bs 0))
  (if (or (>= as (length a))
          (>= bs (length b)))
      t
    (if (= (- (length a) as) (- (length b) bs))
        (if (eq (aref a as) (aref b bs))
            (string= a b (+ 1 as) (+ 1 bs))))))
        
(defvar *SYMBOL-SPECIALS* ".,:;-~!@#$%^&*_=+\\/?<>|")
(defvar *SPECIALS* "()[]\"'`[]")
#+:sbcl
(defvar *SPACES* (format nil "~c~c~c~c" #\space #\newline #\tab #\return))

(defun symbol-special? (c)
  (not (eq nil (index-of c *SYMBOL-SPECIALS*))))

(defun special? (c)
  (not (eq nil (index-of c *SPECIALS*))))

(defun space? (c)
  (not (eq nil (index-of c *SPACES*))))

(defun digit? (c)
  (and (>= c (char-code #\0))
       (<= c (char-code #\9))))

(defun lower-alpha? (c)
  (and (>= c (char-code #\a))
       (<= c (char-code #\z))))

(defun upper-alpha? (c)
  (and (>= c (char-code #\A))
       (<= c (char-code #\Z))))

(defun alpha? (c)
  (or (lower-alpha? c)
      (upper-alpha? c)))

(defun symbol-char? (c)
  (or (alpha? c)
      (digit? c)
      (symbol-special? c)))

(defun special-form? (symbol-offset)
  (let ((sym (symbol-string symbol-offset)))
    (or (string= sym "if")
        (string= sym "let")
        (string= sym "set")
        (string= sym "quote")
        (string= sym "values")
        (string= sym "multiple-value-bind")
        (string= sym "lambda"))))

;; todo detect if the symbol was already read, return that offset and the original token-offset
(defun read-symbol-inner (str output &optional starting)
  (let ((c (ptr-read-byte str)))
    (cond
     ((symbol-char? c)
      (ptr-write-byte c output)
      (read-symbol-inner (+ 1 str) (+ 1 output) (or starting output)))
     (t (ptr-write-byte 0 output)
        (values 'symbol starting str (+ 1 output))))))

(defun read-symbol (str output)
  (multiple-value-bind (kind value offset token-offset)
                       (read-symbol-inner str output)
                       (let ((sym-id (symbol-id output)))
                         (if sym-id
                             (values kind sym-id offset output)
                           (values kind value offset token-offset)))))
                           
(defun digit-value (c)
  (cond
   ((digit? c) (- c (char-code #\0)))
   ((lower-alpha? c) (+ 10 (- c (char-code #\a))))
   ((upper-alpha? c) (+ 10 (- c (char-code #\A))))
   (t 0)))

(defun read-decimal (str acc base token-offset position)
  (let ((c (ptr-read-byte str)))
    (cond
     ((digit? c)
      (read-decimal (+ 1 str) (+ acc (/ (digit-value c) (expt base position))) base token-offset (+ 1 position)))
     (t (values 'float (float acc) str token-offset))))
  )

(defun read-number (str acc base token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
     ((digit? c)
      (read-number (+ 1 str) (+ (* base acc) (digit-value c)) base token-offset))
     ((eq c (char-code #\.)) (read-decimal (+ 1 str) acc base token-offset 1))
     (t (values 'integer acc str token-offset)))))

(defun read-negative-number (str acc base token-offset)
  (let ((c (ptr-read-byte str)))
    (if (digit? c)
        (multiple-value-bind (kind value offset token-offset)
                             (read-number str acc base token-offset)
                             (values kind (- value) offset token-offset))
      (read-symbol (- str 1) token-offset))))

(defun read-plus (str token-offset)
  (let ((c (ptr-read-byte (+ 1 str))))
    (cond
     ((digit? c) (read-number (+ 1 str) 0 *NUMBER-BASE* token-offset))
     (t (read-symbol str token-offset)))))

(defun read-token (str token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
     ((space? c) (read-token (+ 1 str) token-offset))
     ((digit? c) (read-number str 0 *NUMBER-BASE* token-offset))
     ((eq c (char-code #\+)) (read-plus str token-offset))
     ((eq c (char-code #\-)) (read-negative-number (+ 1 str) 0 *NUMBER-BASE* token-offset))
     ((or (alpha? c)
          (symbol-special? c)) (read-symbol str token-offset))
     ((special? c) (values 'special c (+ 1 str) token-offset))
     ((null? c) (values 'eos nil (+ 1 str) token-offset))
     (t (values 'unknown nil (+ 1 str) token-offset)))))

(defvar *SIZEOF_SHORT* 2)
(defvar *SIZEOF_LONG* 4)
(defvar *SIZEOF_FLOAT* 4)
(defvar *REGISTER-SIZE* *SIZEOF_LONG*)

;;;
;;; The compiler
;;;

(defun make-short (&optional a b c d)
  (logior (or a 0)
          (ash (or b 0) 4)
          (ash (or c 0) 8)
          (ash (or d 0) 12)))

(defun make-op (op &optional a b c)
  (case op
        ('load (make-short #x5 a b c))
        ('store (make-short #xD a b c))
        ('call (make-short #x7 #x7 b c))
        ('ret (make-short #x7 #xc a b))
        ('push (make-short #xE a b c))
        ('pop (make-short #x6 a b c))
        ('dec (make-short #X9 a b c))
        (t (error 'unknown-op-error :op op)))
  )

(defun emit-op (stack op &optional a b c)
  (format *standard-output* "~A ~A ~A ~A~%" op (or a 0) (or b 0) (or c 0))
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
      (emit-float (emit-op asm-stack 'load 0 0 15) value)
      (emit-integer (emit-op asm-stack 'load 0 0 15) value)))

(defun emit-lookup-call (asm-stack symbol env-start env)
  ;; load the symbol value into R1 and the toplevel lookup from the top of the stack
  ;; todo need to lookup the lookup function since lambad's set env to env-start
  (format *standard-output* ";;  globally~%")
  (emit-integer (emit-op (emit-integer (emit-op (emit-integer (emit-op asm-stack 'load 1 0 15)
                                                              symbol)
                                                'load 0 0 11)
                                       (- env env-start))
                         'call 0)
                0))

(defun env-push-binding (name env)
  (ptr-write-long name env)
  (+ *SIZEOF_LONG* env))

(defun env-pop-bindings (env num)
  (if (> num 0)
      (env-pop-bindings (- env *SIZEOF_LONG*) (- num 1))
    env))

(defun env-symbol-position (sym env-start env &optional (n 0))
  (if (>= env env-start)
      (if (eq (ptr-read-long env) sym)
          (- n 1)
        (env-symbol-position sym env-start (- env *SIZEOF_LONG*) (+ 1 n)))
    nil))

(defun emit-load-stack-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack 'load register 0 11) (* *REGISTER-SIZE* offset))
  )

(defun emit-store-stack-value (asm-stack offset &optional (register 0))
  (emit-integer (emit-op asm-stack 'store register 0 11) (* *REGISTER-SIZE* offset))
  )

(defun emit-lookup (asm-stack symbol env-start env)
  ;; search env for symbol
  ;; if found, get its stack position and emit code to load its value
  ;; otherwise try the global lookup function
  (format *standard-output* ";; Lookup ~A ~A~%" (symbol-string symbol) env)
  (let ((stack-pos (env-symbol-position symbol env-start env)))
    (if stack-pos
        (emit-load-stack-value asm-stack stack-pos)
      (emit-lookup-call asm-stack symbol env-start env))))

;; todo adjust stack offset in env with each push
(defun compile-call-argument (str code-segment asm-stack token-offset env-start env arg)
  ;; until an #\) is read
  ;;   call each argument
  (format *standard-output* ";; Argument ~A~%" arg)
  (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env token-kind token-value)
                       (repl-compile-inner str code-segment asm-stack token-offset env-start env)
                       (if (and (eq token-kind 'special) (eq token-value (char-code #\))))
                           (values offset code-segment new-asm-stack new-token-offset env arg)
                         ;; push result onto stack and move to the next argument
                         (compile-call-argument offset code-segment (emit-op new-asm-stack 'push 0) new-token-offset env-start (env-push-binding 0 new-env) (+ 1 arg))))
  )

(defun emit-call (asm-stack args &optional (n args))
  (if (<= n 0)
      (emit-integer (emit-op (emit-op asm-stack 'pop 0) 'call 0 0) 0)
    (emit-call (emit-op asm-stack 'pop n) args (- n 1))))

(defun compile-call-it (str code-segment asm-stack token-offset env-start env args)
  (format *standard-output* ";; Call it: ~A args~%" args)
  (values str code-segment (emit-call asm-stack args) token-offset (env-pop-bindings env args)))

(defun compile-funcall (str code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env args)
                       ;; pop results into that argument's register
                       (compile-call-argument str code-segment asm-stack token-offset env-start env 0)
                       ;; make the call
                       (compile-call-it offset code-segment new-asm-stack new-token-offset env-start new-env args)
  ))

(defun compile-if (offset code-segment asm-stack token-offset env-start env)
  (error 'not-implemented-error :feature 'IF :offset offset)
  (values offset code-segment asm-stack token-offset env))

(defun emit-poppers (asm-stack num-bindings)
  (format *standard-output* ";; ~A poppers~%" num-bindings)
  (emit-integer (emit-op asm-stack 'dec 11) (* num-bindings *REGISTER-SIZE*)))

(defun compile-progn (offset code-segment asm-stack token-offset env-start env &optional (n 0))
  (format *standard-output* ";; expr ~A~%" n)
  (multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
                       (repl-compile-inner offset code-segment asm-stack token-offset env-start env)
                       (if (and (eq kind 'special) (eq value (char-code #\))))
                           (values offset code-segment asm-stack token-offset env)
                         (compile-progn offset code-segment asm-stack token-offset env-start env (+ 1 n)))))

;;; LET
(defun compile-let-body (num-bindings offset code-segment asm-stack token-offset env-start env &optional (n 0))
  (multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
                       (compile-progn offset code-segment asm-stack token-offset env-start env)
                       (format *standard-output* ";; let body closing~%")
                       (values offset
                               code-segment
                               (emit-poppers asm-stack num-bindings)
                               token-offset
                               (env-pop-bindings env num-bindings))
                       ))

(defun compile-let-binding-initializer (num name start-offset code-segment asm-stack token-offset env-start env)
  (format *standard-output* ";;  ~A: ~A init~%" num (symbol-string name))
  (multiple-value-bind (offset code-segment asm-stack token-offset env)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env)
                       (multiple-value-bind (kind value offset token-offset)
                                            (read-token offset token-offset)
                                            (unless (and (eq kind 'special) (eq value (char-code #\))))
                                              (error 'malformed-let-error :offset start-offset))
                                            (values
                                             offset
                                             code-segment
                                             ;; push to stack
                                             (emit-call asm-stack 'push 0)
                                             token-offset
                                             ;; add to env
                                             (env-push-binding name env)))))

(defun compile-let-binding (num start-offset code-segment asm-stack token-offset env-start env)
  ;; read the name
  (multiple-value-bind (kind name offset token-offset)
                       (read-token start-offset token-offset)
                       (unless (eq kind 'symbol)
                         (error 'malformed-let-error :offset start-offset))
                       ;; compile the initializer
                       (compile-let-binding-initializer num name offset code-segment asm-stack token-offset env-start env)))

(defun compile-let-bindings (num start-offset code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ;; read #\(
                        ((and (eq kind 'special) (eq value (char-code #\()))
                         (multiple-value-bind (offset code-segment asm-stack token-offset env)
                                              (compile-let-binding num offset code-segment asm-stack token-offset env-start env)
                                              (compile-let-bindings (+ 1 num) offset code-segment asm-stack token-offset env-start env))
                         )
                        ((and (eq kind 'special) (eq value (char-code #\))))
                         (format *standard-output* ";; Let body, ~A bindings~%" num)
                         (compile-let-body num offset code-segment asm-stack token-offset env-start env)
                         )
                        (t (error 'malformed-let-error :offset start-offset)))
                       )
  )

(defun compile-let (start-offset code-segment asm-stack token-offset env-start env)
  ;; todo
  ;; for each binding, compile and push the value, then push the name's symbol value to env
  ;; compile the body
  ;; clean up the stack
  (format *standard-output* ";; LET~%")
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (if (and (eq kind 'special) (eq value (char-code #\()))
                           (compile-let-bindings 0 offset code-segment asm-stack token-offset env-start env)
                         (error 'malformed-let-error :offset start-offset))))

;; Lambda
(defun compile-lambda-optional-bindings (num start-offset code-segment asm-stack token-offset env-start env func-name)
  ;; read token, determine symbol or list, and push symbols or list's head into env, compile list's tail and conditionally run before the body
  (error 'not-implemented-error)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       ))

(defun compile-lambda-rest-binding (num start-offset code-segment asm-stack token-offset env-start env func-name)
  ;; read symbol and push into env, then move to the body
  (error 'not-implemented-error)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       ))

(defun compile-lambda-body (num-bindings start-offset code-segment asm-stack token-offset env-start env func-name)
  (multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
                       (compile-progn start-offset code-segment asm-stack token-offset env-start env)
                       (format *standard-output* ";; lambda closing ~A~%" (symbol-string func-name))
                       (if func-name (setq num-bindings (+ 1 num-bindings)))
                       (values offset
                               code-segment
                               (emit-op (emit-poppers asm-stack num-bindings)
                                        'ret)
                               token-offset
                               (env-pop-bindings env num-bindings))
                       )
  )

(defun compile-lambda-bindings (num start-offset code-segment asm-stack token-offset env-start env func-name)
  ;; read symbols and push into env until &optional or &rest or )
  ;; todo shift env-start?
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ((and (eq kind 'symbol) (string= (symbol-string value) "&optional"))
                         (compile-lambda-optional-bindings num offset code-segment asm-stack token-offset env-start env func-name))
                        ((and (eq kind 'symbol) (string= (symbol-string value) "&rest"))
                         (compile-lambda-rest-binding num offset code-segment asm-stack token-offset env-start env func-name))
                        ((eq kind 'symbol)
                         (format *standard-output* ";;   ~A: ~A~%" num (symbol-string value))
                         (compile-lambda-bindings (+ 1 num) offset code-segment (emit-op asm-stack 'push (+ 1 num)) token-offset env-start (env-push-binding value env) func-name))
                        ((and (eq kind 'special) (eq value (char-code #\))))
                         (compile-lambda-body num offset code-segment asm-stack token-offset env-start env func-name))
                        (t (error 'malformed-lambda-error :offset start-offset))))
  )

(defun compile-lambda (start-offset code-segment orig-asm-stack token-offset env-start env &optional name)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ((and (eq name nil) (eq kind 'symbol))
                         (compile-lambda offset code-segment (emit-op orig-asm-stack 'push 12) token-offset env-start (env-push-binding value env) value))
                        ((and (eq kind 'special) (eq value (char-code #\()))
                         (format *standard-output* ";; Lambda ~A~%" (symbol-string name))
                         (multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
                                              (compile-lambda-bindings 0 offset code-segment orig-asm-stack token-offset env env name)
                                              (format *standard-output* ";; Copied to code-segment ~A~%" code-segment)
                                              (values offset
                                                      ;; copy code from asm-stack to code-segment
                                                      (ptr-copy orig-asm-stack code-segment (- asm-stack orig-asm-stack))
                                                      ;; emit the start in the code-segment on the asm-stack at the original asm-stack
                                                      (emit-value orig-asm-stack 'integer code-segment)
                                                      token-offset
                                                      env-start
                                                      env)))
                        (t (error 'malformed-lambda-error :offset start-offset)))))

;; SET
(defun compile-set-local (name stack-pos start-offset code-segment asm-stack token-offset env-start env)
  ;; if local, compile and store the value
  (format *standard-output* ";; setting local ~A at SP+~A~%" (symbol-string name) (* *SIZEOF_LONG* stack-pos))
  (multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env)
                       (if kind (error 'malformed-error :offset start-offset :msg kind))
                       ;; eat the terminating )
                       (multiple-value-bind (kind value offset token-offset)
                                            (read-token offset token-offset)
                                            (if (and (eq kind 'special) (eq value (char-code #\))))
                                                (values offset code-segment (emit-store-stack-value asm-stack stack-pos 0) token-offset env)
                                              (error 'malformed-error :offset start-offset :msg "Terminator")))
  ))

(defun compile-set-global (name offset code-segment asm-stack token-offset env-start env)
  ;; if global, compile, and add or set the value
  (format *standard-output* ";; setting global ~A~%" (symbol-string name))
  ;(multiple-value-bind (offset code-segment asm-stack token-offset env kind value)
  ;                     (repl-compile-inner offset code-segment asm-stack token-offset env-start env)
  (error 'not-implemented-error :feature 'set :offset offset)
  ;(values offset code-segment asm-stack token-offset env)
  )

(defun compile-set (offset code-segment asm-stack token-offset env-start env)
  ;; read symbol
  (multiple-value-bind (kind value offset token-offset)
                       (read-token offset token-offset)
                       ;; determine if variable is on the stack or a global
                       (let ((stack-pos (env-symbol-position value env-start env)))
                         (if stack-pos
                             (compile-set-local value stack-pos offset code-segment asm-stack token-offset env-start env)
                           (compile-set-global value offset code-segment asm-stack token-offset env-start env)))))

(defun compile-quote (start-offset code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (kind quoted-value offset token-offset)
                       (read-token start-offset token-offset)
                       (if (eq kind 'symbol)
                           (multiple-value-bind (kind value offset token-offset)
                                                (read-token offset token-offset)
                                                (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
                                                (if (and (eq kind 'special) (eq value (char-code #\))))
                                                    (values offset code-segment (emit-value asm-stack 'integer quoted-value) token-offset env)
                                                  (error 'malformed-error :offset start-offset)))
                         (error 'not-yet-implemented-error :feature 'quote :offset start-offset))))

(defun compile-shortcut-quote (start-offset code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (kind quoted-value offset token-offset)
                       (read-token start-offset token-offset)
                       (if (eq kind 'symbol)
                           (progn
                             (format *standard-output* ";; Quote: ~A~%" (symbol-string quoted-value))
                             (values offset code-segment (emit-value asm-stack 'integer quoted-value) token-offset env))
                         (error 'malformed-error :offset start-offset))))


(defun compile-values (offset code-segment asm-stack token-offset env-start env)
  ;; can be a runtime function w/o call, preamble, and postamble; but does return from the caller: defop?
  (error 'not-implemented-error :feature 'values :offset offset)
  (values offset code-segment asm-stack token-offset env))

(defun compile-mvb (offset code-segment asm-stack token-offset env-start env)
  (error 'not-implemented-error :feature 'multi-value-bind :offset offset)
  (values offset code-segment asm-stack token-offset env))

(defun compile-special-form (form offset code-segment asm-stack token-offset env-start env)
  (let ((form-str (symbol-string form)))
    (cond
     ((string= form-str "if") (compile-if offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "let") (compile-let offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "set") (compile-set offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "quote") (compile-quote offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "values") (compile-values offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "multiple-value-bind") (compile-mvb offset code-segment asm-stack token-offset env-start env))
     ((string= form-str "lambda") (compile-lambda offset code-segment asm-stack token-offset env-start env))
     (t (error 'unknown-special-form-error :offset offset :form form-str)))))

(defun compile-call (str code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token str token-offset)
                       (format *standard-output* ";; Calling ~A ~A~%" kind (if (eq kind 'symbol) (symbol-string value) value))
                       (cond
                        ;; special forms
                        ((and (eq kind 'symbol) (special-form? value))
                         (compile-special-form value offset code-segment asm-stack token-offset env-start env))
                        ;; function calls
                        ((or (eq kind 'symbol))
                         (compile-funcall offset code-segment (emit-op (emit-lookup asm-stack value env-start env) 'push 0) token-offset env-start env))
                        ;; or call function by address
                        ((or (eq kind 'integer))
                         (compile-funcall offset code-segment (emit-op (emit-value asm-stack 'integer value) 'push 0) token-offset env-start env))
                        (t (error 'compile-call-error))))
  )

(defun repl-compile-inner (str code-segment asm-stack token-offset env-start env)
  (multiple-value-bind (kind value offset new-token-offset)
                       (read-token str token-offset)
                       (cond
                        ((eq kind 'integer) (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env))
                        ((eq kind 'float) (values offset code-segment (emit-value asm-stack 'float value) new-token-offset env))
                        ((eq kind 'symbol) (values offset code-segment (emit-lookup asm-stack value env-start env) new-token-offset env))
                        ((and (eq kind 'special) (eq value (char-code #\())) (compile-call offset code-segment asm-stack new-token-offset env-start env))
                        ((and (eq kind 'special) (eq value (char-code #\'))) (compile-shortcut-quote offset code-segment asm-stack new-token-offset env-start env))
                        ((eq kind 'special) (values offset code-segment asm-stack new-token-offset env kind value))
                        (t (error 'invalid-token-error :offset str :kind kind :value value))
                        ))
  )

(defun repl-compile (str code-segment asm-stack token-offset env-start env)
  (setf *TOKEN-SEGMENT* token-offset)
  (repl-compile-inner str code-segment asm-stack token-offset env-start env)
  )

;;;
;;; Test functions
;;;

#-:sbcl
(defun assert (v)
  (if (not v)
      (error 'assertion-failed)))

(defun assert-read-token (offset kind value new-offset token-offset &optional new-token-offset)
  (multiple-value-bind (k v o to) (read-token offset token-offset)
                       (assert (eq k kind))
                       (assert (eq v value))
                       (assert (eq o new-offset))
                       (if new-token-offset (assert (eq to new-token-offset)))))

(defun assert-read-symbol (offset value new-offset token-offset)
  (assert-read-token offset 'symbol token-offset new-offset token-offset (+ 1 token-offset (length value)))
  (assert (string= (ptr-read-string token-offset) value)))

(defun assert-read-integer (offset value new-offset token-offset)
  (assert-read-token offset 'integer value new-offset token-offset token-offset))

(defun assert-read-float (offset value new-offset token-offset)
  (assert-read-token offset 'float value new-offset token-offset token-offset))

(defun assert-read-special (offset char new-offset token-offset)
  (assert-read-token offset 'special (char-code char) new-offset token-offset token-offset))

(defun test-read-token ()
  (ptr-write-string "123" 0)
  (assert-read-integer 0 123 3 1024)
  (ptr-write-string "-123.45" 0)
  (assert-read-float 0 -123.45 7 1024)
  (ptr-write-string "-hello" 0)
  (assert-read-symbol 0 "-hello" 6 1024)
  (ptr-write-string "hello (world) 123 -34 + +45 +hello" 0)
  (assert-read-symbol 0 "hello" 5 1024)
  (assert-read-special 5 #\( 7 1024)
  (assert-read-symbol 7 "world" 12 (+ 1024 6))
  (assert-read-special 12 #\) 13 (+ 1024 6))
  (assert-read-integer 13 123 17 (+ 1024 6))
  (assert-read-integer 17 -34 21 (+ 1024 6))
  (assert-read-symbol 21 "+" 23 (+ 1024 6 2))
  (assert-read-integer 23 45 27 (+ 1024 6 2))
  (assert-read-symbol 27 "+hello" 34 (+ 1024 6 2 7))
  )

(defun test-read-self ()
  ;; read this source
  ;; see if read never errors
  )

(defun print-string (start)
  (let* ((str (ptr-read-string start))
         (len (length str)))
    (if (> len 0)
        (progn
          (format *standard-output* "~A  ~A~%" start str)
          len))))

(defun print-strings (start size)
  (if (> size 0)
      (let ((len (print-string start)))
        (if len
            (print-strings (+ start len) (- size len))))))

(defun debug-compile (o-offset o-code-segment o-asm-stack o-token-offset env-start o-env)
  (multiple-value-bind (offset code-segment asm-stack token-offset env)
                       (repl-compile o-offset o-code-segment o-asm-stack o-token-offset env-start o-env)
                       (format *standard-output* "~A ~A ~A ~A ~A~%" offset code-segment asm-stack token-offset env)
                       (print 'Input)
                       (print-string o-offset)
                       (print (ptr-read-array o-offset (- offset o-offset)))
                       (print 'Code-Segment)
                       (print (ptr-read-array o-code-segment (- code-segment o-code-segment)))
                       (print 'Asm-Stack)
                       (print (ptr-read-array o-asm-stack (- asm-stack o-asm-stack)))
                       (print 'Strings)
                       (print-strings o-token-offset (- token-offset o-token-offset))
                       (print (ptr-read-array o-token-offset (- token-offset o-token-offset)))
                       (print 'Env)
                       (print (ptr-read-array env-start (- env env-start)))))

(defun test-compile ()
  (ptr-write-string "(print 'value (+ x 1 2 (* 3 4 (square x)) 5))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-quote ()
  (ptr-write-string "(quote value)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-let ()
  (ptr-write-string "(let ((x (+ 1 2))
                           (y (* 3 4 (square x))))
                       (print x)
                       (print y)
                       (print (+ x y)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-set-local ()
  (ptr-write-string "(let ((x 0)
                           (y 0))
                       (set x (+ 1 123))
                       123.45
                       (set y 456)
                       (let ((z 1))
                         (set z 2)
                         (set x 456)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-lambda ()
  (ptr-write-string "(lambda (x y)
                       (print x y)
                       (set x (+ 1 123))
                       (set y 456))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-named-lambda ()
  (ptr-write-string "(lambda f (x y)
                       (print y x)
                       (set x (+ 1 123))
                       (set y 456)
                       (f (+ 1 x) y))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test-compile-set-lambda ()
  (ptr-write-string "(set f (lambda (n)
                       (print n)
                       (f (- 1 n))))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004))

(defun test ()
  (test-read-token))
