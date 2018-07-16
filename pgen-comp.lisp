;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; A parser generator with operations like BNF or regular expressions.
;;; Similar to Ragel, YACC, BNF, and regular expressions.
;;; See: (documentation 'define-parser 'function)

(defpackage :pgen
  (:use :cl)
  (:export :statement-error
           :define-parser))

(in-package :pgen)

(define-condition statement-error ()
  ((statement :initarg :statement :initform nil))
  (:report (lambda (condition stream)
             (format stream "Statement Error: ~A~%" (slot-value condition 'statement)))))

(defun safe-elt (seq n)
  (if (and seq (< 0 (length seq)))
      (elt seq n)))

(defmacro named-lambda (name args &body body)
  `(labels ((,name ,args ,@body))
     #',name))

(defun rule-name (rule)
  (first rule))

(defun rule-statements (rule)
  (rest rule))

(defun rule-func (prefix rule-name)
  "Prepares a string with the name of the function generated for a rule."
  (string-upcase (concatenate 'string (if (symbolp prefix)
                                          (symbol-name prefix)
                                          prefix)
                              "-"
                              (if (symbolp rule-name)
                                  (symbol-name rule-name)
                                  rule-name))))

(defun compile-rule-call (prefix rule)
  "Statements that are symbols call rules. Since rules are implemented with functions they need to be called."
  `(,(intern (rule-func prefix rule)) input))

(defun compile-or (prefix statement &optional (num 0))
  "An `(or A B C)` statement matches either A, B, or C in that order. If A doesn't match, then B is tried, and then C."
  (if statement
      `(multiple-value-bind (value input)
           ,(compile-statement prefix num (first statement))
         (if value
             (values value input)
             ,(compile-or prefix (rest statement) (+ 1 num))))
      `(values nil input)))

(defun compile-any-match ()
  "Matches anything. Like the other statement compilers, this returns the first element of the input and the remaining input."
  `(values (safe-elt input 0) (subseq input 1)))

(defun compile-exact-match (item)
  "An exact match is any character or integer statement. Exactness is tested with EQ. Like the other statement compilers, this returns the matched element and the remaining input."
  `(if (eq ,item (safe-elt input 0))
       (values (safe-elt input 0) (subseq input 1))
       (values nil input)))

(defun compile-int-range (min max)
  "Compiles a (range min max) statement when given integers. Any element in the input inclusively between MIN and MAX will result in a match returning the matched element the remaining input."
  `(let ((c (safe-elt input 0)))
     (if (and c
              (<= ,min c)
              (>= ,max c))
         (values c (subseq input 1))
         (values nil input))))

(defun compile-range (min max)
  "Compiles a (range min max) statement when given characters. If the char-code of the current element is between char-codes of MIN and MAX, then the matched element and the remaining input is returned."
  `(let* ((el (safe-elt input 0))
          (c (if el (char-code el) 0)))
     (if (and c
              (<= ,(char-code min) c)
              (>= ,(char-code max) c))
         (values c (subseq input 1))
         (values nil input))))

(defun compile-match-eos ()
  "Matches the end of the sequence."
  `(if (or (null input) (= 0 (length input)))
       (values t input)
       (values nil input)))

(defun exec-zero-or-one (fn input)
  (multiple-value-bind (value next-input)
      (funcall fn input)
    (if value
        (values (first value) next-input)
        (values t input))))

(defun compile-zero-or-one (prefix statements)
  "Compiles `(? statement...)` statements. Returns the result and remaining input if the statements matched, but when they don't it still returns T as the matched value without advancing the input."
  `(exec-zero-or-one #'(lambda (input &optional acc)
                         ,(compile-statements prefix statements))
                     input))

(defun exec-one-or-more (fn input &optional acc)
  (multiple-value-bind (value next-input)
      (funcall fn input)
    (if value
        (exec-one-or-more fn next-input (cons value acc))
        (values (reverse acc) next-input))))

(defun compile-one-or-more (prefix statements)
  "Compiles `(+ statement...)` statements. It matches one or more subsequences that match the statements. Each match is placed into a list and returned along with the remaining input."
  `(exec-one-or-more #'(lambda (input &optional acc)
                         ,(compile-statements prefix statements))
                     input))

(defun exec-zero-or-more (fn input &optional acc)
  (multiple-value-bind (value next-input)
      (funcall fn input)
    (if value
        (exec-zero-or-more fn next-input (cons value acc))
        (values (or (reverse acc) t) next-input))))

(defun compile-zero-or-more (prefix statements)
  "Compiles `(* statement...)` statements. It matches zero or more subsequences that match the statements. Each match is placed into a list and returned along with the remaining input. When nothing matches T is the returned value with the input remaining the same."
  `(exec-zero-or-more #'(lambda (input &optional acc)
                          ,(compile-statements prefix statements))
                      input))

(defun compile-call (prefix statement)
  "Compiles any (OP args...) statement."
  (case (first statement)
    (cl-user::quote (compile-exact-match (first (rest statement))))
    (cl-user::range (if (characterp (first (rest statement)))
                        (compile-range (first (rest statement)) (second (rest statement)))
                        (compile-int-range (first (rest statement)) (second (rest statement)))))
    (cl-user::or (compile-or prefix (rest statement)))
    (cl-user::+ (compile-one-or-more prefix (rest statement)))
    (cl-user::* (compile-zero-or-more prefix (rest statement)))
    (cl-user::? (compile-zero-or-one prefix (rest statement)))
    (otherwise (error 'statement-error :statement statement))))

(defun compile-statement-body (prefix num statement)
  (cond
    ((eq statement nil) (compile-match-eos))
    ((eq statement 'cl-user::any) (compile-any-match))
    ((symbolp statement) (compile-rule-call prefix statement))
    ((atom statement) (compile-exact-match statement))
    ((listp statement) (compile-call prefix statement))
    (t (error 'statement-error :statement statement))))

(defun compile-statement (prefix num statement)
  "Compiles an individual statement in a rule."
  (compile-statement-body prefix num statement))

(defun action-arglist (n &optional acc)
  "Actions assumed variables named $0, $1, $2, etc. based on the number of preceding statements' returned values. This creates that arglist."
  (if (>= n 0)
      (action-arglist (- n 1) (cons (intern (format nil "$~A" n)) acc))
      acc))

(defun reverse-value-accumulator (acc)
  (if (atom acc)
      (list acc)
      (reverse acc)))


(defun compile-action (num action)
  "Actions get wrapped into a lambda that provides the variable bindings for the values. The action's return value becomes the new value that is returned."
  (cond
    ((symbolp action) `(let ((racc (reverse-value-accumulator acc)))
                         (values (apply ,action (cons racc racc)) input)))
    ((listp action) `(let ((racc (reverse-value-accumulator acc)))
                       (values (apply #'(lambda ,(action-arglist num)
                                          (declare (ignorable ,@(action-arglist num)))
                                          ,action)
                                      (cons racc racc))
                               input)))
    (t (error 'statement-error :statement action))))

(defun compile-at-action (num action)
  (cond
    ((symbolp action) `(let ((racc (reverse-value-accumulator acc)))
                         (apply ,action (cons racc racc))))
    ((listp action) `(let ((racc (reverse-value-accumulator acc)))
                       (apply #'(lambda ,(action-arglist num)
                                  (declare (ignorable ,@(action-arglist num)))
                                  ,action)
                              (cons racc racc))))
    (t (error 'statement-error :statement action))))

(defun compile-at-actions-inner (num actions &optional acc)
  (if actions
      (compile-at-actions-inner num (rest actions) (cons (compile-at-action num (first actions)) acc))
      (reverse acc)))

(defun compile-at-actions (num actions)
  "@Actions get called after all the statements in a rule get matched. They don't affect the value list, but do get passed the numbered bindings."
  `(progn ,@(compile-at-actions-inner num actions)
          (values acc input)))

(defun compile-rule-block (statement code-acc)
  "This compiles pre-processed statement (KIND . COMPILED-EXPR) pair into code by creating calls to each statement and testing the if it matched. Depending on the statement's KIND, the result value is either replaced or added to the list of values."
  (case (first statement)
    (replace
     ;; replaces the stack of values with the action's return value
     `(multiple-value-bind (value next-input)
          ,(rest statement)
        (if value
            (let ((input next-input)
                  (acc value))
              ,(or code-acc '(values value input)))
            (values nil input))))
    ;; statements and insert actions add values to the stack
    ((statement insert)
     `(multiple-value-bind (value next-input)
          ,(rest statement)
        (if value
            (let ((input next-input)
                  (acc (cons value acc)))
              ,(or code-acc '(values acc
                              input)))
            (values nil input))))
    (otherwise (error 'statement-error :statement statement))))

(defun compile-rule-body (num statements actions &optional code-acc)
  "After compiling the statements into pairs of (KIND . COMPILED-EXPR) this works through each statement accumulating code."
  (if actions
      (compile-rule-body num statements nil (compile-at-actions num actions))
      (if statements
          (compile-rule-body num
                             (rest statements)
                             actions
                             (compile-rule-block (first statements) code-acc))
          code-acc)))

(defun compile-statements-inner (prefix statements &optional (num 0) code-acc actions)
  "Turns a rule's statement list into pairs of (KIND . COMPILED-EXPR). The compiled expression is what tests if the input matches and what remains. Not really useful without #'COMPILE-RULE-BODY."
  (cond
    ((eq (first statements) 'cl-user::$)
     (compile-statements-inner prefix
                               (rest (rest statements))
                               (+ 1 num)
                               (cons (cons 'insert (compile-action num (first (rest statements)))) code-acc)))
    ((eq (first statements) 'cl-user::>)
     (compile-statements-inner prefix
                               (rest (rest statements))
                               1
                               (cons (cons 'replace (compile-action num (first (rest statements)))) code-acc)))
    ((eq (first statements) 'cl-user::@)
     (compile-statements-inner prefix
                               (rest (rest statements))
                               num
                               code-acc
                               (cons (first (rest statements)) actions)))
    ((null statements)
     (values code-acc num actions))
    (t
     (compile-statements-inner prefix
                               (rest statements)
                               (+ 1 num)
                               (cons (cons 'statement (compile-statement prefix num (first statements))) code-acc)))))

(defun compile-statements (prefix statements)
  "Fully turns a list of statement into an executable expression."
  (multiple-value-bind (code-acc num actions)
      (compile-statements-inner prefix statements)
    (compile-rule-body num code-acc actions)))


(defun compile-rule (prefix rule)
  "Turns a list of the form (rule-name statement...) into a callable function named PREFIX-RULE-NAME."
  (let ((rule-func-name (rule-func prefix (rule-name rule))))
    `(defun ,(intern rule-func-name) (input)
       (let ((acc nil))
         (multiple-value-bind (value next-input)
             ,(compile-statements prefix (rule-statements rule))
           (if value
               (values (if (listp value)
                           (reverse value)
                           value)
                       next-input)
               (values nil input)))))))


(defun compile-rules (name rules &optional top-rule code)
  "Compiles each rule in a list of rules."
  (if (symbolp name) (setf name (symbol-name name)))
  (if rules
      (compile-rules name
                     (rest rules)
                     (or top-rule (rule-name (first rules)))
                     (cons (compile-rule name (first rules)) code))
      (values top-rule code)))

(defmacro define-parser (name &rest body)
  "Defines a new function NAME that runs the rules against a string or any sequence depending on the rules. The accumulated matches and what remains of the input sequence are returned using VALUES. A Statement-Error is raised when the rules fail to compile.

Each rule gets its own function that runs its statements. The RULES list contains lists of the form (name statements...). Statements either match or perform an action.

The matchers are:
  * Character or number: exact match by character or integer value
  * nil to match the end of the sequence
  * any to match anything
  * Other symbols call the rule with that name
  * (range min max): Matches any character between min and max
  * (? statements...): The statements MAY be present
  * (* statements...): Match zero or many
  * (+ statements...): Match one or many
  * (or statements...): Matches any of the statements

Actions are a symbol that specifies when the action is performed followed by a function name to call, \"> print\", or a list to evaluate, \"> (print $1)\".

Action prefixes are:
  * >  Run the action when encountered. The action's return value replaces all the preceding values.
  * $  Run the action when encountered. The action's return value gets appended to the value list.
  * @  Run the action after all the statements have matched.

When evaluating an action list, the preceding matches are accessable using variables $1 through $N. $0 is a list of all the matches.

Example integer parser:

  ;; Helper function

  (defun digit-list-to-number (lst &optional neg (n 0) (base 10))
    \"Returns an integer whose digits are each element of the list processed MSB first.\"
    (if lst (digit-list-to-number (rest lst) neg (+ (* n base) (first lst)))
      (if neg (- n) n)))

  ;; The actual parser

  (define-parser test-parse-number
    \"Parses a signed integer from a string.\"
    ((number (? #\-) (+ digit) > (digit-list-to-number (mapcar #'first $2) (not (eq $1 T))))
     (digit (range #\0 #\9) >(- $1 (char-code #\0)))))

  ;; Example usage

  (multiple-value-bind (value next-input)
     (test-parse-number \"-123abc\")
     (print value) ; => -123
     (print next-input)) => \"abc\"

  (test-parse-number-expand) ; => returns the expanded macro
  test-parse-number-rules ; => copy of the parser's rules
"
  (let ((doc-string (if (stringp (first body))
                        (first body)
                        (format nil "Calls a generated parser that matches STR using the ~A rules. STR can be a string or a sequence depending on the parser. Refer to ~A-RULES for a stored copy of the rules." name name)))
        (rules (if (stringp (first body))
                   (second body)
                   (first body))))
    (multiple-value-bind (top-rule code)
        (compile-rules (symbol-name name) rules)
      `(progn
         ,@code
         (defvar ,(intern (rule-func name "rules")))
         (setf ,(intern (rule-func name "rules")) ',rules)
         (defun ,(intern (rule-func name "expand")) () (macroexpand '(define-parser ,name ,rules)))
         (defun ,name (str)
           ,doc-string
           (,(intern (rule-func name top-rule)) str)))))
  )
