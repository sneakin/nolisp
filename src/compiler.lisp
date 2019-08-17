;;;
;;; Error conditions
;;;

(define-condition nc-error (error)
  ((msg :initarg :msg :initform "Error")
   (form :initarg :form :initform nil)
   (state :initarg :state :initform nil))
  (:report (lambda (condition stream)
             (format stream "~s:~%Form: ~S~%State: ~S~%"
                     (nc-error-msg condition)
                     (nc-error-form condition)
                     (nc-error-state condition)))))

(define-condition nc-unknown-error (nc-error) ())
(define-condition nc-macro-exists-error (nc-error) ())
(define-condition nc-undefined-error (nc-error) ())

;;;
;;; The code walker
;;;

(defun nc-code-walker (form atom-visitor list-visitor
                       &optional
                         state
                         (recurser #'(lambda (f &optional (s state))
                                       (nc-code-walker f atom-visitor list-visitor s))))
  (cond
    ((atom form) (funcall atom-visitor form state))
    ((listp form) (funcall list-visitor form recurser state))
    (t (error 'nc-unknown-error :form form))))

;;;
;;; Utility functions
;;;
(defun flatten (lst &optional result (top t))
  (if lst
      (if (atom lst)
          (cons lst result)
          (flatten (rest lst) (flatten (first lst) result nil) top))
      (if top
          (nreverse result)
          result)))

(defun clip-last (lst)
  (let ((rl (reverse lst)))
    (values (nreverse (rest rl))
            (first rl))))

(defun nshift-left (lst)
  (rplacd (last lst) (cons (first lst) nil))
  (rest lst))

(defun shift-left (lst)
  (append (rest lst) (list (first lst))))

(defun shift-right (lst)
  (nreverse (shift-left (reverse lst))))

(defun ifeq (a)
  #'(lambda (x)
    (eq x a)))

(defun range-inner (max &optional (min 0) (step 1) acc)
  (if (> max min)
      (range-inner (- max step) min step (cons max acc))
      acc))

(defun range (max &optional (min 0) (step 1))
  (range-inner (if (> max min) max min)
               (if (> max min) min max)
               (if (> max min) step (- step))))

(defun curry (fn &rest args)
  #'(lambda (&rest more)
      (apply fn (append more args))))

;;;
;;; Macros
;;;

(defvar *macros* '())

(defun macro? (form)
  (not (not (assoc form *macros*))))

(defun nc-remove-macro (name)
  (let ((macro (assoc name *macros*)))
    (if macro
        (setq *macros* (remove macro *macros*)))))

(defun nc-add-macro (name fn)
  ;(if (special-form? name) (error 'nc-macro-exists-error :form name))
  (push (cons name fn) *macros*)
  fn)

(defun nc-update-macro (name fn)
  (nc-remove-macro name)
  (nc-add-macro name fn))

(defun gen-macro-caller (args body)
  (eval `(lambda ,args ,@body)))

(defun nc-defmacro (form env)
  (nc-update-macro (first form) (gen-macro-caller (second form) (rest (rest form))))
  nil)

(defun nc-call-macro (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *macros*)))
    (if macro
        (apply (cdr macro) args)
        (mapcar visitor form))))

(defun nc-macroexpand-1 (form)
  (nc-code-walker form
                  #'(lambda (a state) a)
                  #'nc-call-macro))

(defun nc-macroexpand (form)
  (let ((new-form (nc-macroexpand-1 form)))
    (if (equal form new-form)
        form
        (nc-macroexpand new-form))))

(nc-update-macro 'LET #'(lambda (bindings &rest body)
                          `(funcall (lambda ,(mapcar #'first bindings)
                                        ,@body)
                                    ,@(mapcar #'second bindings))))

;;;
;;; Symbol collector
;;;

(defun nc-remove-syms (args form)
  (if args
      (nc-remove-syms (rest args) (remove (first args) form))
      form))

(defvar *nc-keywords* '(nil IF LET DEFUN PROGN ARGN))

(defun nc-collect-open-bindings (form &optional (env *nc-keywords*))
  (nc-remove-syms env
                  (nc-code-walker form
                                  #'(lambda (a state) (if (symbolp a) a))
                                  #'(lambda (form visitor state)
                                      (if (eq 'LAMBDA (first form))
                                          (nc-remove-syms (second form)
                                                          (flatten (mapcar visitor (rest (rest form)))))
                                          (flatten (mapcar visitor form)))
                                      ))))

;;;
;;; Linearizing
;;;



;;;
;;; CPS Transform
;;;

(defun cps-atom? (form)
  (or (atom form)
      (eq 'LAMBDA (first form))
      (eq 'λ (first form))))

(defun cps-wrap (sym form cc)
  (cond
    ((or (null cc) (eq sym cc)) form)
    ((cps-atom? cc) `(,@form ,cc))
    (t `(,@form (λ (,sym) ,cc)))))

(defun cps-lambda (sym form)
  `(λ (,sym) ,form))

(defun nc-cps-transform-call-emit (fns ops visitor &optional (first-call t))
  (if fns
      (let* ((form (rest (first fns)))
             (sym (first (first fns)))
             (new-form (funcall visitor form (cps-lambda sym ops))))
        (nc-cps-transform-call-emit (rest fns)
                                    new-form
                                    visitor
                                    nil))
      ops))

(defvar *something-else* nil)
(defvar *something* nil)

(defun lambda-form? (form)
  (and (listp form) (eq 'LAMBDA (first form))))

(defun nc-cps-transform-call-inner (form visitor state &optional args fns (sym (gensym "R")))
  (if form
      (let ((tip (first form)))
        ;; (format *standard-output* "CP ~S ~S~%" form state)
        (if (cps-atom? tip)
            (nc-cps-transform-call-inner (rest form) visitor state
                                         (cons (if (lambda-form? tip)
                                                   (funcall visitor tip)
                                                   tip)
                                               args)
                                         fns
                                         sym)
            (nc-cps-transform-call-inner (rest form) visitor state
                                         (cons sym args)
                                         (acons sym tip fns)
                                         (gensym "R"))))
      (nc-cps-transform-call-emit fns
                                  (cps-wrap sym (reverse args) state)
                                  visitor)))

(defun nc-cps-transform-call (form visitor state)
  (nc-cps-transform-call-inner form visitor state))

(defun nc-cps-transform-list (form visitor state)
  (cond
    ((null form) state)
    ((eq (first form) 'IF)
     (funcall visitor (second form) `(λ (test)
                                        (if test
                                            ,(funcall visitor (third form) state)
                                            ,(funcall visitor (fourth form) state)))))
    ((eq (first form) 'LAMBDA) (let ((cc (gensym "CC"))
                                     (fp (gensym "FP")))
                                 `(LAMBDA (,cc ,fp ,@(second form))
                                    ,(funcall visitor (if (fourth form)
                                                          (rest (rest form))
                                                          (third form))
                                              cc))))
    ((eq (first form) 'DEFUN) (let ((cc (gensym "CC")))
                                `(DEFUN ,(second form) (,cc ,@(third form))
                                   ,(funcall visitor (if (fifth form)
                                                         (rest (rest (rest form)))
                                                         (fourth form))
                                             cc))))
    (form (nc-cps-transform-call form visitor state)
          )))

(defun uncurry-first (form value)
  (let ((body (subst value (first (second form)) (third form))))
    (if (eq 1 (length (second form)))
        body
        `(,(first form) ,(rest (second form))
           ,body))))

(defun nc-cps-transform-lookup (atom state)
  (cond
    ((null state) atom)
    ((atom state) (list state atom))
    ((eq 'λ (first state)) (uncurry-first state atom))
    (state (list state atom))
    (t (error 'nc-cps-error :form form :state state))))

(defun nc-cps-transform (form &optional (state 'RETURN))
  (nc-code-walker form #'nc-cps-transform-lookup #'nc-cps-transform-list state))

(load "assert-match.lisp")

(defun test-cps-transform ()
  (assert-matches '((abc (return abc))
                    ((boo 1 2 3) (boo 1 2 3 RETURN))
                    ((boo (who))
                     (who (λ (:R) (boo :R RETURN))))
                    ((lambda (x) x) (lambda (:return :fp x) (:return x)))
                    ((lambda (x) (* x x)) (lambda (:return :fp x) (* x x :return)))
                    ((if (> a b) (boo a) (boo b))
                     (> a b (λ (TEST)
                               (if TEST
                                   (boo a RETURN)
                                   (boo b RETURN)))))
                    ((if a (boo a) (boo b))
                     (if a
                         (boo a RETURN)
                         (boo b RETURN)))
                    ((if (> a b) a b)
                     (> a b (λ (TEST)
                               (if TEST
                                   (RETURN a)
                                   (RETURN b)))))
                    ((if x (boo)) (if x (boo return) (return nil)))
                    ((who (if (> a b) (boo a) b))
                     (> a b (λ (TEST)
                               (if TEST
                                   (boo a (λ (:R) (who :R RETURN)))
                                   (who b RETURN)))))
                    ((lambda (x y) (if (> x y) x y))
                     (lambda (:return :fp x y)
                       (> x y (λ (TEST) (if TEST (:return x) (:return y))))))
                    ((boo (lambda (x) (who x)) 3)
                     (boo (lambda (:return :fp x) (who x :return)) 3 RETURN))
                    ((boo (if night you me))
                     (if night (boo you RETURN) (boo me RETURN)))
                    ((defun fn (x) (* x x))
                     (defun fn (:return x) (* x x :return))))
                  :fn #'nc-cps-transform))

;;;
;;; Variable lookup in call frames
;;;

(defun make-frame (args &optional parent)
  (cons args parent))
(defun frame-parent (frame)
  (cdr frame))
(defun frame-lookup-1 (frame sym)
  (position sym (first frame)))

(defun frame-lookup (frame sym &optional (depth 0))
  (if frame
      (let ((index (frame-lookup-1 frame sym)))
        (if index
            (values index depth)
            (frame-lookup (frame-parent frame) sym (+ depth 1))))
      ))

(defun nc-lookup-walker-atom (form state &optional (depth 0))
  (multiple-value-bind (index depth)
      (frame-lookup state form)
    (if index
        (if (eq depth 0)
            (list 'ARGN index)
            (list 'LOOKUP depth index))
        form)))

(defun nc-lookup-walker-call (visitor state name args &optional ops)
  (if args
      (nc-lookup-walker-call visitor state
                             name (rest args)
                             (cons (funcall (curry visitor state) (first args)) ops))
      (nreverse ops)))

(defun nc-lookup-walker-list (form visitor state)
  (cond
    ((eq 'LAMBDA (first form))
     (let ((args (second form))
           (body (rest (rest form))))
       `(LAMBDA ,args
          ,@(mapcar (curry visitor (make-frame args state))
                    body)))
     )
    ((eq 'λ (first form))
     (let ((args (second form))
           (body (rest (rest form))))
       `(λ ,args
          ,@(mapcar (curry visitor (make-frame args state))
                    body)))
     )
    ((eq 'DEFUN (first form))
     `(DEFUN ,(second form) ,(third form)
        ,@(mapcar (curry visitor (make-frame (third form)))
                  (rest (rest (rest form))))))
    (t (if (or (eq 'ARGN (first form))
               (eq 'LOOKUP (first form)))
           form
           (cons (first form)
                 (nc-lookup-walker-call visitor state (first form) (rest form))
                 )))))

(defun nc-lookup-walker (form)
  (nc-code-walker form
                  #'nc-lookup-walker-atom
                  #'nc-lookup-walker-list))

(defun test-lookup-walker ()
  (assert-matches '((x x)
                    ((defun f (x y)
                      (* y y (λ (r0) (* x r0 RETURN))))
                     (defun f (x y)
                      (* (argn 1) (argn 1)
                         (λ (:R) (* (lookup 1 0) (argn 0) RETURN))))
                     "lambda forms add a frame")
                    ((lambda (ra fp x) (ra x)) (lambda (ra fp x) (ra (argn 2)))
                     "adds a closing frame pointer")
                    ((if x (return a) (return b))
                     (if x (return a) (return b))
                     "unknown symbols pass through")
                    ((λ (x y) (a x (λ (R0) (b x (λ (R1) (* y R0 R1))))))
                     (λ (x y) (a (argn 0)
                                 (λ (R0) (b (lookup 1 0)
                                            (λ (R1) (* (lookup 2 1)
                                                       (lookup 1 0)
                                                       (argn 0)))))))
                     "nested lambdas as arguments add a frame")
                    ((λ (x) (/ x y
                               (λ (R0)
                                  (to-int R0 (λ (R1)
                                                (if R1
                                                    (return x)
                                                    (return y)))))))
                     (λ (x) (/ (argn 0) y
                               (λ (:R0)
                                  (to-int (argn 0)
                                          (λ (:R1)
                                             (if (argn 0)
                                                 (return (lookup 2 0))
                                                 (return y)))))))
                     "nested lambda forms increase lookup depth"))
                  :fn #'nc-lookup-walker))
;;
;; Forth walker
;;

(defvar *forth-forms* '())

(defun nc-add-forth-form (name fn)
  (push (cons name fn) *forth-forms*)
  fn)

(defun nc-remove-forth-form (name)
  (let ((macro (assoc name *forth-forms*)))
    (if macro
        (setq *forth-forms* (remove macro *forth-forms*)))))

(defun nc-update-forth-form (name fn)
  (nc-remove-forth-form name)
  (nc-add-forth-form name fn))

(defun nc-forthgen-arg-loaders (args &optional ops (offset 0))
  (if args
      (let ((item (first args)))
        (if (atom item)
            (nc-forthgen-arg-loaders (rest args)
                                     (cons (first args) ops)
                                     (+ offset 1))
            (let* ((new-offset (+ offset 1 (if (atom item) 1 (length item)))))
              (nc-forthgen-arg-loaders (rest args)
                                       (if (> offset 0)
                                           (cons `(,offset overn) ops)
                                           ops)
                                       new-offset))))
      
      (nreverse ops)))

(defun nc-forthgen-funcall (visitor state fn args &optional ops cont)
  (if args
      (let* ((new-state state)
             (new-visitor (curry visitor new-state))
             (forth-form (funcall new-visitor (first args))))
        (nc-forthgen-funcall visitor new-state fn (rest args)
                             (if cont (cons forth-form ops) ops)
                             (if cont cont forth-form)))
      `(,@ops ,fn ,cont)))

(defun nc-forthgen-list (form visitor state)
  (let* ((name (first form))
         (args (rest form))
         (macro (assoc name *forth-forms*)))
    (if macro
        (apply (cdr macro) (cons visitor args))
        (nc-forthgen-funcall visitor state name (shift-right args)))))

(defun nc-forthgen-lookup (sym state)
  (identity sym)
  ;; (if (eq sym 'RETURN)
  ;;     nil
  ;;     (identity sym))
  )

(defun nc-forthgen (form)
  (nc-code-walker form
                  #'nc-forthgen-lookup
                  #'nc-forthgen-list))

(nc-update-forth-form 'IF #'(lambda (visitor test then else)
                              `(,(funcall visitor test) IF :newline
                                 ,(funcall visitor then) ELSE :newline
                                 ,(funcall visitor else) THEN :newline)))
(nc-update-forth-form 'DEFUN #'(lambda (visitor name args &rest body)
                                 `(":" ,name "(" CC ,@(rest args) ")" :newline
                                       begin-frame :newline
                                       ,@(mapcar visitor body)
                                       frame-return :newline ";")))
(nc-update-forth-form 'λ #'(lambda (visitor args &rest body)
                                 `("(" ,@args ")" :newline
                                       tail-frame :newline
                                       ,@(mapcar visitor body))))
(nc-update-forth-form 'LAMBDA #'(lambda (visitor args &rest body)
                                  `("[" begin-frame  "(" CC FP ,@(rest (rest args)) ")" :newline
                                        ,@(mapcar visitor body)
                                        frame-return :newline
                                        "]" close-lambda :newline)))
(nc-update-forth-form 'PROGN #'(lambda (visitor &rest calls)
                                 (mapcar visitor calls)))
(nc-update-forth-form 'RETURN #'(lambda (visitor arg)
                                  (funcall visitor arg)))
(nc-update-forth-form 'ARGN #'(lambda (visitor n)
                                `(ARGN> ,n)))
(nc-update-forth-form 'LOOKUP #'(lambda (visitor depth n)
                                `(LOOKUP> ,depth ,n)))

(defun nc-forthgen-arg-reverser (op)
  #'(lambda (visitor &rest args)
      (multiple-value-bind (lst last-item)
          (clip-last args)
        (mapcar visitor (append lst (list op last-item))))))

(dolist (op '(+ - * / < <= >= > ^ **))
  (nc-update-forth-form op (nc-forthgen-arg-reverser op)))

;;;
;;; Compiler
;;;

(defun nc-list-compile (form)
  (nc-forthgen (nc-lookup-walker (nc-cps-transform (nc-macroexpand form)))))

(defun nc-compile (form)
  (flatten (nc-forthgen (nc-lookup-walker (nc-cps-transform (nc-macroexpand form))))))

(defun nc-to-string (form)
  (format nil "~{~A ~}" (substitute "" :var (substitute "" :call (substitute (format nil "~%") :newline form)))))

;;;
;;; Test cases
;;;

(load "nassert.lisp")

(defun setup-test ()
  (nc-add-macro 'boo #'(lambda (x y) `(+ ,x ,y)))
  (nc-add-macro 'who #'(lambda (x y) `(boo ,x ,y))))

(defun teardown-test ()
  (nc-remove-macro 'boo)
  (nc-remove-macro 'who)
  nil)

(defun test-nc-macroexpand-1 ()
  (setup-test)
  (assert-equal (nc-macroexpand-1 '(who (boo 3 4) (who (boo 4 5) 7)))
                '(boo (+ 3 4) (boo (+ 4 5) 7)))
  (teardown-test))

(defun test-nc-macroexpand ()
  (setup-test)
  (assert-equal (nc-macroexpand '(who (boo 3 4) (who (boo 4 5) 7)))
                '(+ (+ 3 4) (+ (+ 4 5) 7)))
  (teardown-test))

(defun test-nc-compile ()
  (assert-matches '((ABC (ABC) "global symbols pass through")
                    (123 (123) "integers pass through")
                    ("Hello" ("Hello") "strings pass through")
                    ((+ 2 (* 3 4) 5)
                     (4 3 *
                      5 1 OVERN
                      2 +)
                     "nested function call")
                    ((+ (* x x)
                      (* y y))
                     (x x *
                      y y *
                      4 overn +)
                     "Nested function call")
                    ((cons :key (list a (cons x y) (list b c (list d e f) g)))
                     (f e d list
                      g 1 overn c b list
                      y x cons
                      c 1 overn a list
                      :key cons))
                    ((f (g 1 (list 1 2 3 4) (allot 16)) (progn 16 32 64 128) 100)
                     (4 3 2 1 list
                      16 allot
                      17 overn 1 g
                      16 32 64 128
                      100 1 overn 6 overn f)
                     "can find arguments after an arbitray stack size increase")
                    ((/ 1 (/ 1 (/ 1 e))) ; todo proper math arg ordering
                     (e 1 /
                      1 /
                      1 /)
                     "doubly nested")
                    ((defun fn ()
                       123)
                     (":" fn "(" ")" :NEWLINE
                      begin-frame :NEWLINE
                      123
                      frame-return :NEWLINE
                      ";")
                     "defun returning a value")
                    ((defun fn (x y)
                       (+ x y))
                     (":" fn "(" x y ")" :NEWLINE
                      begin-frame :NEWLINE
                      1 argn 0 argn +
                      frame-return :NEWLINE
                      ";")
                     "defun with one call")
                    ((defun mag (x y)
                       (+ (* x x) (* y y)))
                     (":" mag "(" x y ")" :NEWLINE
                      begin-frame :NEWLINE
                      0 argn 0 argn * :NEWLINE
                      1 argn 1 argn * :NEWLINE
                      2 overn 3 overn +
                      frame-return :NEWLINE
                      ";")
                     "defun with one call")
                    ((defun fn (x) (- 2 (if (> x 3) 3 4)))
                     (":" FN "(" CC X ")" :NEWLINE
                      BEGIN-FRAME :NEWLINE
                      ARGN> 1 3 > "(" TEST ")" :NEWLINE
                      TAIL-FRAME :NEWLINE
                      ARGN> 0 IF :NEWLINE
                      2 3 - LOOKUP> 1 0 ELSE :NEWLINE
                      2 4 - LOOKUP> 1 0 THEN :NEWLINE
                      FRAME-RETURN :NEWLINE
                      ";")
                     "subtraction and comparisons get swapped")
                    ((defun squarer () (lambda (x) (* x x)))
                     (":" squarer "(" ?ra ")" :newline
                      begin-frame :newline
                      "[" begin-frame "(" ?ra1 ?fp x ")":newline
                      0 argn
                      2 argn 2 argn *
                      frame-return :newline
                      "]" current-frame close-lambda :newline
                      frame-return :newline
                      ";")
                     "function returning an anonymous function"))
                  :fn #'nc-compile
                  :allow-keywords nil))
