(defpackage :repl
  (:use :cl)
  (:export :undefined-variable-error
           :unknown-op-error
           :malformed-error
           :malformed-let-error
           :env-define
           :env-symbol-position
           :env-stack-position
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
           :ptr-find-string=
           :symbol-string
           :symbol-id
           :space?
           :null?
           :symbol-char?
           :special?
           :alpha?
           :digit?
           :special-form?
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
           :test-compile-if
           :test-compile-asm
           :test-compile-values
           :test-write-to-array
           :test-write
           :string=
           :repl-read
           :make-op
           :make-short
           :emit-lookup
           :emit-lookup-call
           :emit-value
           :repl-compile
           :write-to-array
           :write-to-file
           ))

(in-package :repl)

(define-condition repl-error ()
  ((offset :initarg :offset :initform nil)
   (msg :initarg :msg :initform nil))
  (:report (lambda (condition stream)
             (format stream "Error at offset ~A~%" (slot-value condition 'offset))
             (if (slot-value condition 'msg)
                 (format stream (slot-value condition 'msg))))))

(define-condition unknown-op-error (repl-error)
  ((op :initarg :op :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown op error: ~A ~A~%" (slot-value condition 'op) (type-of (slot-value condition 'op))))))

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
             (format stream "Invalid ~A token at ~A: ~A~%"
                     (slot-value condition 'kind)
                     (slot-value condition 'offset)
                     (slot-value condition 'value)))))

(define-condition undefined-error (repl-error)
  ((name :initarg :name :initform nil))
  (:report (lambda (condition stream)
             (format stream "Undefined variable ~A~%"
                     (slot-value condition 'name)))))

(define-condition undefined-variable-error (undefined-error) ())
(define-condition undefined-function-error (undefined-error) ())

;;;
;;; Compatibility layer functions
;;;

;;; All input comes in through *MEMORY* and any array values like symbols
;;; get written to *MEMORY* as well.
(defvar *MEMORY* (make-array (* 8 1024) :element-type '(unsigned-byte 8)))
(defvar *TOKEN-SEGMENT* 0)
(defvar *CODE-SEGMENT* 0)
;;; Base of the read numbers
(defvar *NUMBER-BASE* 10)

(defun ptr-read-byte (ptr)
  (aref *MEMORY* ptr))

(defun ptr-write-byte (c ptr)
  (setf (aref *MEMORY* ptr) c)
  (+ ptr 1))

(defun ptr-read-array (ptr elements &optional (arr (make-array elements :element-type '(unsigned-byte 8))))
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

(defun ptr-find-string= (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (> (length current) 0)
            (if (string= current str)
                stack-start
              (ptr-find-string= str (+ 1 stack-start (length current)) stack-end))))))

(defun ptr-find-string-equal (str stack-start stack-end)
  (if (< stack-start stack-end)
      (let ((current (ptr-read-string stack-start)))
        (if (> (length current) 0)
            (if (string-equal current str)
                stack-start
              (ptr-find-string-equal str (+ 1 stack-start (length current)) stack-end))))))

(defun ptr-zero (offset count)
  (if (> count 4)
      (progn
        (ptr-write-long 0 offset)
        (ptr-zero (+ offset *SIZEOF_LONG*) (- count *SIZEOF_LONG*)))
    (if (> count 0)
        (progn
          (ptr-write-byte 0 offset)
          (ptr-zero (+ offset 1) (- count 1)))
      offset)))

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

(defun symbol-id (symbol-offset &optional (segment *TOKEN-SEGMENT*))
  (ptr-find-string-equal (ptr-read-string symbol-offset) segment symbol-offset))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string symbol-offset)))

(defun symbol-intern (str start ending)
  (let* ((off (ptr-write-string str ending))
         (id (symbol-id off start)))
    (if id id off)))

#-:sbcl
(defun string= (a b &optional (as 0) (bs 0))
  (if (or (>= as (length a))
          (>= bs (length b)))
      t
    (if (= (- (length a) as) (- (length b) bs))
        (if (eq (aref a as) (aref b bs))
            (string= a b (+ 1 as) (+ 1 bs))))))
        
(defvar *SYMBOL-SPECIALS* ".,:-~!@#$%^&*_=+\/?<>|")
(defvar *SPECIALS* "()[]\"'`[]")
#+:sbcl
(defvar *SPACES* (format nil "~c~c~c~c" #\space #\newline #\tab #\return))

(defun symbol-special? (c)
  (not (eq nil (index-of c *SYMBOL-SPECIALS*))))

(defun special? (c)
  (not (eq nil (index-of c *SPECIALS*))))

(defun newline? (c)
  (or (eq c (char-code #\newline)) (eq c (char-code #\return))))

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
    (or (string-equal sym "if")
        (string-equal sym "let")
        (string-equal sym "set")
        (string-equal sym "quote")
        (string-equal sym "values")
        (string-equal sym "apply-values")
        (string-equal sym "multiple-value-bind")
        (string-equal sym "lambda")
        (string-equal sym "asm")
        (string-equal sym "progn"))))

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

(defun read-comment (str)
  (if (newline? (ptr-read-byte str))
      (+ 1 str)
    (read-comment (+ 1 str))))

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
     ((eq c (char-code #\;)) (read-token (read-comment (+ 1 str)) token-offset))
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
   (:LOAD (make-short #x5 a b c))
   (:STORE (make-short #xD a b c))
   (:CALL (make-short #x7 #x7 a b))
   (:RET (make-short #x7 #xc a b))
   (:PUSH (make-short #xE a b c))
   (:POP (make-short #x6 a b c))
   (:INC (make-short #x1 a b c))
   (:ADDI (make-short 2 1 a b))
   (:CLS (make-short 0 #xe a b))
   (:DEC (make-short #x9 a b c))
   (:MOV (make-short #x8 a b c))
   (:CMP (make-short #x2 0 a b))
   (:RESET (make-short #x7 #x1 a b))
   (t (error 'unknown-op-error :op op))))

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
      (emit-float (emit-op asm-stack :load 0 0 15) value)
      (emit-integer (emit-op asm-stack :load 0 0 15) value)))

(defun env-push-binding (name env)
  (ptr-write-long name env)
  (+ *REGISTER-SIZE* env))

(defun env-pop-bindings (env num)
  (if (> num 0)
      (env-pop-bindings (- env *REGISTER-SIZE*) (- num 1))
    env))

(defun env-symbol-position (sym env-start env &optional (n 0))
  (if (> env env-start)
      (if (eq (ptr-read-long (- env *REGISTER-SIZE*)) sym)
          n
        (env-symbol-position sym env-start (- env *REGISTER-SIZE*) (+ 1 n)))
    nil))

(defun env-data-position (sym env-start env)
  (let ((pos (env-symbol-position sym env-start env)))
    (if pos
        (- (/ (- env env-start) *REGISTER-SIZE*) pos 1))))

(defun env-stack-position (sym env-start env)
  (env-symbol-position sym env-start env))

(defun env-define (name env-start env)
  (let ((pos (env-symbol-position name env-start env)))
    (if pos
        env
      (env-push-binding name env))))

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

(defun emit-lookup (asm-stack symbol env-start env toplevel-start toplevel)
  (let ((new-asm (emit-lookup-local asm-stack symbol env-start env)))
    (if new-asm
        new-asm
      (let ((new-asm (emit-lookup-global asm-stack symbol toplevel-start toplevel)))
        (if new-asm
            new-asm
          ;; todo call lookup for runtime use?
          (error 'undefined-variable-error :name (symbol-string symbol)))))))

;; todo adjust stack offset in env with each push
(defun compile-call-argument (str code-segment asm-stack token-offset env-start env toplevel-start toplevel arg)
  ;; until an #\) is read
  ;;   call each argument
  (format *standard-output* ";; Argument ~A~%" arg)
  (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env toplevel token-kind token-value)
                       (repl-compile-inner str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if (and (eq token-kind 'special) (eq token-value (char-code #\))))
                           (values offset code-segment new-asm-stack new-token-offset env toplevel arg)
                         ;; push result onto stack and move to the next argument
                         (compile-call-argument offset code-segment (emit-op new-asm-stack :push 0) new-token-offset env-start (env-push-binding 0 new-env) toplevel-start toplevel (+ 1 arg))))
  )

(defun emit-reg-call (asm-stack reg)
  (emit-integer (emit-op (emit-op (emit-op asm-stack
                                           :cls #x7)
                                  :addi 10 14)
                         :call 0 reg) 0)  )

(defun emit-call  (asm-stack reg offset)
  (emit-reg-call (emit-integer (emit-op asm-stack :load 0 0 reg)
                               offset)
                 offset))

(defun emit-stack-call (asm-stack offset)
  (emit-call asm-stack 11 offset))

(defun emit-pop-values (asm-stack reg num)
  (if (> num 0)
      (emit-pop-values (emit-op asm-stack :pop (+ 1 (- num reg))) reg (- num 1))
    asm-stack))

(defun emit-funcall (asm-stack reg data-offset args &optional (n args))
  (emit-call (emit-pop-values asm-stack 1 n) reg data-offset))

(defun compile-funcall-it (str code-segment asm-stack token-offset env-start env toplevel-start toplevel data-offset reg args)
  (format *standard-output* ";; Call R~A+~A: ~A args~%" reg (* *REGISTER-SIZE* data-offset) args)
  (values str code-segment (emit-funcall asm-stack reg (* *REGISTER-SIZE* data-offset) args) token-offset (env-pop-bindings env args) toplevel))

(defun compile-funcall-int (str code-segment asm-stack token-offset env-start env toplevel-start toplevel reg data-offset)
  (multiple-value-bind (offset code-segment new-asm-stack new-token-offset new-env toplevel args)
                       ;; pop results into that argument's register
                       (compile-call-argument str code-segment asm-stack token-offset env-start env toplevel-start toplevel 0)
                       ;; make the call
                       (compile-funcall-it offset code-segment new-asm-stack new-token-offset env-start new-env toplevel-start toplevel data-offset reg args)
                       ))

(defun compile-funcall (func-name str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (let ((stack-pos (env-stack-position func-name env-start env))
        (data-pos (env-data-position func-name toplevel-start toplevel)))
    (if stack-pos
        (compile-funcall-int str code-segment asm-stack token-offset env-start env toplevel-start toplevel 11 stack-pos)
      (if data-pos
          (compile-funcall-int str code-segment asm-stack token-offset env-start env toplevel-start toplevel 9 data-pos)
        (error 'undefined-function :offset str :name (symbol-string func-name))))))

;(emit-op (emit-lookup asm-stack value env-start env toplevel-start toplevel) :push 0)

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

(defun compile-if-else (if-offset then-offset start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; compile the ELSE form
  (format *standard-output* ";; IF else~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       ;; no form, then done
                       (if (and (eq kind 'special) (eq value (char-code #\))))
                           (compile-if-fixer if-offset nil offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                         ;; almost done
                         (if kind
                             (error 'invalid-token-error :offset start-offset :kind kind :value value)
                           (compile-if-closing if-offset then-offset offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))
                           ))
;;;(error 'invalid-token-error :offset start-offset :kind kind :value value)

(defun compile-if-then (if-offset start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; compile the THEN form
  (format *standard-output* ";; IF then~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       ;; no form, then done
                       (if (and (eq kind 'special) (eq kind (char-code #\))))
                           (compile-if-fixer if-offset nil offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                         ;; form so try ELSE
                         (if kind
                             (error 'invalid-token-error :offset start-offset :kind kind :value value)
                           (compile-if-else if-offset
                                          (+ *SIZEOF_SHORT* asm-stack)
                                          offset
                                          code-segment
                                          (emit-integer (emit-op asm-stack :inc 12) #xFFFFFFFF)
                                          token-offset
                                          env-start
                                          env
                                          toplevel-start
                                          toplevel)))))

(defun compile-if (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; compile the test
  (format *standard-output* ";; IF condition~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if kind (error 'invalid-token-error :offset start-offset :kind kind :value value))
                       (compile-if-then (+ (* 2 *SIZEOF_LONG*) *SIZEOF_SHORT* asm-stack)
                                        offset
                                        code-segment
                                        (emit-integer (emit-op (emit-op (emit-integer (emit-op asm-stack :load 1 0 15) 0) :cmp 0 1) :inc 12 #x1) #xFFFFFFFF)
                                        token-offset env-start env toplevel-start toplevel)))

(defun emit-poppers (asm-stack num-bindings)
  (format *standard-output* ";; ~A poppers~%" num-bindings)
  (if (> num-bindings 0)
      (emit-integer (emit-op asm-stack :inc 11) (* num-bindings *REGISTER-SIZE*))
    asm-stack))

(defun compile-progn (offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional (n 0))
  (format *standard-output* ";; expr ~A~%" n)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if (and (eq kind 'special) (eq value (char-code #\))))
                           (values offset code-segment asm-stack token-offset env toplevel)
                         (compile-progn offset code-segment asm-stack token-offset env-start env toplevel-start toplevel (+ 1 n)))))

(defun compile-toplevel (start-offset o-code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel &optional (n 0) starting-code-segment starting-asm-stack)
  (format *standard-output* ";; toplevel ~A ~A~%" n o-code-segment)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset o-code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel)
                       (if (and (eq kind 'eos))
                           ;; copy code to code-segment from asm-stack
                           (let* ((asm-stack (emit-op asm-stack :ret))
                                  (new-code-segment (ptr-copy starting-asm-stack code-segment (- asm-stack starting-asm-stack))))
                             (format *standard-output* ";;    code segment ~A ~A ~A ~A~%" o-code-segment code-segment *code-segment* (- code-segment *code-segment*))
                             (values offset
                                     
                                     new-code-segment
                                     ;; emit the address for toplevel's initializer
                                     (emit-value starting-asm-stack 'integer (- code-segment *code-segment*))
                                     token-offset
                                     env
                                     toplevel))
                         (if (eq kind nil)
                             (compile-toplevel offset code-segment asm-stack token-offset env-start env toplevel-start toplevel (+ 1 n) (or starting-code-segment o-code-segment) (or starting-asm-stack orig-asm-stack))
                           (error 'invalid-token-error :offset start-offset :kind kind :value value)))))

;;; LET
(defun compile-let-body (num-bindings offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional (n 0))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (compile-progn offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (format *standard-output* ";; let body closing~%")
                       (values offset
                               code-segment
                               (emit-poppers asm-stack num-bindings)
                               token-offset
                               (env-pop-bindings env num-bindings)
                               toplevel)
                       ))

(defun compile-let-binding-initializer (num name start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";;  ~A: ~A init~%" num (symbol-string name))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (multiple-value-bind (kind value offset token-offset)
                                            (read-token offset token-offset)
                                            (unless (and (eq kind 'special) (eq value (char-code #\))))
                                              (error 'malformed-let-error :offset start-offset))
                                            (values
                                             offset
                                             code-segment
                                             ;; push to stack
                                             (emit-op asm-stack :push 0)
                                             token-offset
                                             ;; add to env
                                             (env-push-binding name env)
                                             toplevel))))

(defun compile-let-binding (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read the name
  (multiple-value-bind (kind name offset token-offset)
                       (read-token start-offset token-offset)
                       (unless (eq kind 'symbol)
                         (error 'malformed-let-error :offset start-offset))
                       ;; compile the initializer
                       (compile-let-binding-initializer num name offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))

(defun compile-let-bindings (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ;; read #\(
                        ((and (eq kind 'special) (eq value (char-code #\()))
                         (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
                                              (compile-let-binding num offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                                              (compile-let-bindings (+ 1 num) offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
                         )
                        ((and (eq kind 'special) (eq value (char-code #\))))
                         (format *standard-output* ";; Let body, ~A bindings~%" num)
                         (compile-let-body num offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                         )
                        (t (error 'malformed-let-error :offset start-offset)))
                       )
  )

(defun compile-let (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; for each binding, compile and push the value, then push the name's symbol value to env
  ;; compile the body
  ;; clean up the stack
  (format *standard-output* ";; LET~%")
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (if (and (eq kind 'special) (eq value (char-code #\()))
                           (compile-let-bindings 0 offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                         (error 'malformed-let-error :offset start-offset))))

;; Lambda
(defun compile-lambda-optional-bindings (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
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

(defun compile-lambda-body (num-bindings start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (compile-progn start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (format *standard-output* ";; lambda closing ~A~%" (symbol-string func-name))
                       (if func-name (setq num-bindings (+ 1 num-bindings)))
                       (values offset
                               code-segment
                               (emit-op (emit-poppers asm-stack num-bindings)
                                        :ret)
                               token-offset
                               (env-pop-bindings env num-bindings)
                               toplevel)
                       )
  )

(defun compile-lambda-bindings (num start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name)
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
                         (compile-lambda-bindings (+ 1 num) offset code-segment (emit-op asm-stack :push (+ 1 num)) token-offset env-start (env-push-binding value env) toplevel-start toplevel func-name))
                        ((and (eq kind 'special) (eq value (char-code #\))))
                         (compile-lambda-body num offset code-segment asm-stack token-offset env-start env toplevel-start toplevel func-name))
                        (t (error 'malformed-lambda-error :offset start-offset))))
  )

(defun compile-lambda (start-offset code-segment orig-asm-stack token-offset env-start env toplevel-start toplevel &optional name)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ;; named lambda, extract the name and push to the stack
                        ((and (eq name nil) (eq kind 'symbol))
                         (compile-lambda offset code-segment (emit-op orig-asm-stack :push 12) token-offset env-start env toplevel-start toplevel value))
                        ;; start of the arglist
                        ((and (eq kind 'special) (eq value (char-code #\()))
                         (format *standard-output* ";; Lambda ~A~%" (symbol-string name))
                         (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                                              (compile-lambda-bindings 0 offset code-segment orig-asm-stack token-offset env (if name (env-push-binding name env) env) toplevel-start toplevel name)
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

;; SET
(defun compile-set-local (name stack-pos start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; if local, compile and store the value
  (format *standard-output* ";; setting local ~A at SP+~A~%" (symbol-string name) (* *SIZEOF_LONG* stack-pos))
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if kind (error 'malformed-error :offset start-offset :msg kind))
                       ;; eat the terminating )
                       (multiple-value-bind (kind value offset token-offset)
                                            (read-token offset token-offset)
                                            (if (and (eq kind 'special) (eq value (char-code #\))))
                                                (values offset code-segment (emit-store-stack-value asm-stack stack-pos 0) token-offset env toplevel)
                                              (error 'malformed-error :offset start-offset :msg "Terminator")))
  ))

(defun compile-set-global (name start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; if global, compile, and add or set the value
  (let ((toplevel (env-define name toplevel-start toplevel)))
    (format *standard-output* ";; setting global ~A ~A ~A ~A~%" (symbol-string name) name toplevel (env-data-position name toplevel-start toplevel))
    (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                         (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                         (if kind (error 'malformed-error :offset start-offset :msg kind))
                         ;; eat the terminating )
                         (multiple-value-bind (kind value offset token-offset)
                                              (read-token offset token-offset)
                                              (if (and (eq kind 'special) (eq value (char-code #\))))
                                                  (let ((data-pos (env-data-position name toplevel-start toplevel)))
                                                    (values offset code-segment (emit-store-data-value asm-stack data-pos 0) token-offset env toplevel))
                                                (error 'malformed-error :offset start-offset :msg "Terminator")))
                         )))

(defun compile-set (offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; read symbol
  (multiple-value-bind (kind value offset token-offset)
                       (read-token offset token-offset)
                       ;; determine if variable is on the stack or a global
                       (let ((stack-pos (env-stack-position value env-start env)))
                         (if stack-pos
                             (compile-set-local value stack-pos offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                           (compile-set-global value offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))))

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


(defun compile-asm-op (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional op a b c)
  ;; each op has up to 4 values: op-name a b c
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ((and (eq kind 'special) (eq value (char-code #\))))
                                        ;(values offset code-segment (emit-op asm-stack (intern (string-upcase (symbol-string op))) a b c) token-offset env toplevel))
                         (compile-asm offset code-segment
                                      (emit-op asm-stack (intern (string-upcase (symbol-string op)) "KEYWORD") a b c)
                                      token-offset env-start env toplevel-start toplevel))
                        ((and (eq kind 'symbol) (eq op nil))
                         (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel value nil nil nil))
                        ((eq kind 'integer)
                         (cond
                          (a (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel op a value nil))
                          (b (compile-asm-op offset code-segment asm-stack token-offset env-start env toplevel-start toplevel op a b value))
                          (c (error 'malformed-error :offset start-offset))
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

(defun compile-values (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional (num 1))
  ;; returns from the caller, keeping arguments in registers
  ;; todo pass register to repl-compile-inner to the mov can be skipped
  ;; todo how to signal that any following forms don't matter? ie: the double ret, if-then's inc?
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel token-kind token-value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (cond
                        ((and (eq token-kind 'special) (eq token-value (char-code #\))))
                         (format *standard-output* ";; returning~%")
                         ;; pop values into register
                         (values offset code-segment
                                 (emit-op (emit-pop-values asm-stack 1 (- num 1)) :mov 0 1)
                                 token-offset env toplevel))
                        ((not token-kind)
                         (format *standard-output* ";; Return value ~A~%" num)
                         ;; push the value and move on
                         (compile-values offset code-segment (emit-op asm-stack :push 0) token-offset env-start env toplevel-start toplevel (+ 1 num)))
                        (t (error 'malformed-error :offset start-offset)))))

(defun compile-apply-values-call (offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (format *standard-output* ";; Apply-Values~%")
  (values offset code-segment (emit-poppers (emit-call asm-stack 11 0) 1) token-offset env toplevel))

;; todo refactor: funcall should be equivalent to (apply-values func (values args...))
(defun compile-apply-values (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  ;; apply-values func expr
  ;; Calls func passing any values return with VALUES as argument values.
  (format *standard-output* ";; Apply-Values~%")
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if kind (error 'malformed-error :offset start-offset))
                       (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind value)
                                            (repl-compile-inner offset code-segment (emit-op asm-stack :push 0) token-offset env-start env toplevel-start toplevel)
                                            (if kind (error 'malformed-error :offset start-offset))
                                            ;; eat terminator
                                            (multiple-value-bind (kind value offset token-offset)
                                                                 (read-token offset token-offset)
                                                                 (unless (and (eq kind 'special) (eq value (char-code #\)))) (error 'invalid-token-error :offset start-offset :kind kind :value value))
                                                                 (compile-apply-values-call offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)))))


(defun compile-mvb-expr (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional num-bindings pre-binding-env)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel kind)
                       (repl-compile-inner start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
                       (if kind (error 'malformed-error :offset start-offset))
                       (compile-mvb-body offset code-segment (emit-pushers asm-stack num-bindings) token-offset env-start env toplevel-start toplevel num-bindings)
                       ))

(defun compile-mvb (start-offset code-segment asm-stack token-offset env-start env toplevel-start toplevel &optional num-bindings pre-binding-env)
  ;; (defmacro multiple-value-bind (bindings expr &rest body)
  ;;   `(apply-values (lambda ,bindings ,@body) ,expr))
  ;; creates a binding in env and pushes the corresponding register
  (multiple-value-bind (kind value offset token-offset)
                       (read-token start-offset token-offset)
                       (cond
                        ((and (eq num-bindings nil) (eq kind 'special) (eq value (char-code #\()))
                         ;; start of binding list
                         (compile-mvb offset code-segment asm-stack token-offset env-start env toplevel-start toplevel 0 env)
                         )
                        ((and (not (eq num-bindings nil)) (eq kind 'special) (eq value (char-code #\()))
                         (error 'malformed-error :offset start-offset))
                        ((and (eq kind 'special) (eq value (char-code #\))))
                         ;; end of binding list
                         ;; compile expression & body
                         (compile-mvb-expr offset code-segment asm-stack token-offset env-start env toplevel-start toplevel num-bindings pre-binding-env)
                         )
                        ((eq kind 'symbol)
                         ;; binding
                         (compile-mvb offset code-segment asm-stack token-offset env-start (env-push-binding value env) toplevel-start toplevel (+ 1 num-bindings) pre-binding-env)
                         )
                        (t (error 'malformed-error :offset start-offset)))))
;(values offset code-segment asm-stack token-offset env toplevel))

(defun compile-special-form (form offset code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (let ((form-str (symbol-string form)))
    (cond
     ((string-equal form-str "if") (compile-if offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "let") (compile-let offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "set") (compile-set offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "quote") (compile-quote offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "values") (compile-values offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "apply-values") (compile-apply-values offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "multiple-value-bind") (compile-mvb offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "lambda") (compile-lambda offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "asm") (compile-asm offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     ((string-equal form-str "progn") (compile-progn offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
     (t (error 'unknown-special-form-error :offset offset :form form-str)))))

(defun compile-call (str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind value offset token-offset)
                       (read-token str token-offset)
                       (format *standard-output* ";; Calling ~A ~A ~A~%" kind (if (eq kind 'symbol) (symbol-string value) value) (special-form? value))
                       (cond
                        ;; special forms
                        ((and (eq kind 'symbol) (special-form? value))
                         (compile-special-form value offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
                        ;; function calls
                        ((eq kind 'symbol)
                         (compile-funcall value offset code-segment asm-stack token-offset env-start env toplevel-start toplevel))
                        ;; or call function by address
                        ((or (eq kind 'integer))
                         (compile-funcall-int offset code-segment (emit-op (emit-value asm-stack 'integer value) :push 0) token-offset env-start env toplevel-start toplevel 9 value))
                        (t (error 'compile-call-error))))
  )

(defun repl-compile-inner (str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (multiple-value-bind (kind value offset new-token-offset)
                       (read-token str token-offset)
                       (cond
                        ((eq kind 'integer) (values offset code-segment (emit-value asm-stack 'integer value) new-token-offset env toplevel))
                        ((eq kind 'float) (values offset code-segment (emit-value asm-stack 'float value) new-token-offset env toplevel))
                        ((eq kind 'symbol) (values offset code-segment (emit-lookup asm-stack value env-start env toplevel-start toplevel) new-token-offset env toplevel))
                        ((and (eq kind 'special) (eq value (char-code #\())) (compile-call offset code-segment asm-stack new-token-offset env-start env toplevel-start toplevel))
                        ((and (eq kind 'special) (eq value (char-code #\'))) (compile-shortcut-quote offset code-segment asm-stack new-token-offset env-start env toplevel-start toplevel))
                        ((or (eq kind 'special) (eq kind 'eos)) (values offset code-segment asm-stack new-token-offset env toplevel kind value))
                        (t (error 'invalid-token-error :offset str :kind kind :value value))
                        ))
  )

(defun repl-compile (str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  (setf *TOKEN-SEGMENT* token-offset)
  (setf *CODE-SEGMENT* code-segment)
  (compile-toplevel str code-segment asm-stack token-offset env-start env toplevel-start toplevel)
  )

(defvar *INIT-FUNC* "__init")

(defun emit-init (asm-stack code-segment data-segment toplevel-start toplevel init)
  (emit-op (emit-reg-call (emit-store-data-value (emit-integer (emit-op
                                                                  (emit-integer (emit-op asm-stack :load 10 0 15)
                                                                                code-segment)
                                                                  :load 9 0 15)
                                                                 data-segment)
                                                   (env-data-position init toplevel-start toplevel))
                            0)
           :reset))

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

(defun write-to-file (path output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel)
  (with-open-file (f path
                     :direction :output
                     :if-exists :supersede
                     :external-format :default
                     :element-type '(unsigned-byte 8))
                  (let* ((output-end (write-to-array output cs-start code-segment asm-start asm-stack token-start token-offset env-start env toplevel-start toplevel))
                         (size (- output-end output)))
                    (format *standard-output* "Size ~A~%" size)
                    (write-sequence (ptr-read-array output size) f))))

(defun compile-to-file (path output o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
                       (repl-compile o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel o-toplevel)
                       (write-to-file path output o-code-segment code-segment o-asm-stack asm-stack o-token-offset token-offset env-start env o-toplevel toplevel)))

;;;
;;; Test functions
;;;

#-:sbcl
(defun assert (v)
  (if (not v)
      (error 'assertion-failed)))

(defun assert-read-token (offset kind value new-offset token-offset &optional new-token-offset)
  (multiple-value-bind (k v o to) (read-token offset token-offset)
                       (format *standard-output* "~A ~A ~A ~A ~A~%" k v t to value)
                       (assert (eq k kind))
                       (assert (eq v value))
                       (assert (eq o new-offset))
                       (if new-token-offset (assert (eq to new-token-offset)))))

(defun assert-read-symbol (offset value new-offset token-offset)
  (setf *TOKEN-SEGMENT* token-offset)
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
  (ptr-write-string (format nil ";; a comment~%123") 0)
  (assert-read-integer 0 123 16 1024)
  (ptr-write-string "hello-world" 0)
  (assert-read-symbol 0 "hello-world" 11 1024)
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
          (+ 1 len)))))

(defun print-strings (start size)
  (if (> size 0)
      (let ((len (print-string start)))
        (if len
            (print-strings (+ start len) (- size len))))))

(defun debug-compile (o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel)
  (multiple-value-bind (offset code-segment asm-stack token-offset env toplevel)
                       (repl-compile o-offset o-code-segment o-asm-stack o-token-offset env-start o-env o-toplevel o-toplevel)
                       (format *standard-output* "~A ~A ~A ~A ~A ~A~%" offset code-segment asm-stack token-offset env toplevel)
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
                       (print (ptr-read-array env-start (- env env-start)))
                       (print 'Toplevel)
                       (print (ptr-read-array o-toplevel (- toplevel o-toplevel)))))

(defun test-compile ()
  (ptr-write-string "(print 'value (+ x 1 2 (* 3 4 (square x)) 5))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-quote ()
  (ptr-write-string "(quote value)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-let ()
  (ptr-write-string "(let ((+ 0) (* 0) (square 0) (print 0)
                           (x (+ 1 2))
                           (y (* 3 4 (square x))))
                       (print x)
                       (print y)
                       (print (+ x y)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-local ()
  (ptr-write-string "(let ((x 0)
                           (y 0))
                       (set x (+ 1 123))
                       123.45
                       (set y 456)
                       (let ((z 1))
                         (set z 2)
                         (set x 456)))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-lambda ()
  (ptr-write-string "(lambda (x y)
                       (print x y)
                       (set x (+ 1 123))
                       (set y 456))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-named-lambda ()
  (ptr-write-string "(lambda f (x y)
                       (print y x)
                       (set x (+ 1 123))
                       (set y 456)
                       (f (+ 1 x) y))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-lambda ()
  (ptr-write-string "(set f (lambda (n)
                       n
                       (f n)))
                     (set g (lambda () (f 10)))
                     (g (f 123))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-set-global ()
  (ptr-write-string "(set x 123)" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-if-2 ()
  (ptr-write-string "(let ((x 1) (t 1) (nil 0)) (if x t nil))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-if ()
  (ptr-write-string "(if 0 1 (progn 1 2 3))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-asm ()
  (ptr-write-string "(asm (push 0))
                     (asm (load 2) 1234 (push 2) (mov 2 0))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-compile-values ()
  (ptr-write-string "(apply-values (lambda (a b c) a) (values 1 2 3))" 0)
  (debug-compile 0 1000 2000 3000 4000 4004 5000))

(defun test-write-to-array ()
  (write-to-array 6000 1000 1050 2000 2006 3000 3009 4000 4004 5000 5004))

(defun test-write (&optional (path "test.out"))
  (ptr-write-string "(set f (lambda (x) (if x (values 1 2 3 4) 255))) (f 123) (f 0)" 0)
  (compile-to-file path 6000 0 1000 2000 3000 4000 4004 5000))

(defun test ()
  (test-read-token))
