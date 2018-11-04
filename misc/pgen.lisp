;;; -*- mode: Lisp; coding: utf-8-unix -*-

(defun safe-elt (seq n)
  (if (and seq (< 0 (length seq)))
      (elt seq n)))

(defun eval-exact-match (value input)
  (let ((c (safe-elt input 0)))
    (format *standard-output* "eval exact match ~A ~A~%" value input)
    (if (eq value c)
        (values c (subseq input 1))
        (values nil input))))

(defun eval-match-eos (input)
  (format *standard-output* "eval match eos ~A~%" input)
  (values (or (null input)
              (= 0 (length input)))
          input))

(defun eval-range (min max input)
  (let ((c (safe-elt input 0)))
    (format *standard-output* "eval range ~A ~A ~A ~A~%" (char-code min) (char-code max) (if c (char-code c)) (if c (<= (char-code min) (char-code c))))
    (if (and c
             (<= (char-code min) (char-code c))
             (>= (char-code max) (char-code c)))
        (values c (subseq input 1))
        (values nil input))))

(defun eval-zero-or-one (rule input rules)
  (format *standard-output* "eval zero or one ~A~%" rule)
  (multiple-value-bind (result new-input)
      (eval-statements nil rule input rules)
    (if result
        (values result new-input)
        (values t input))))

(defun eval-one-or-more (rule input rules &optional acc)
  (format *standard-output* "eval one or more ~A ~A~%" rule acc)
  (multiple-value-bind (result new-input)
      (eval-statements nil rule input rules)
    (if result
        (eval-one-or-more rule new-input rules (cons result acc))
        (values (reverse acc) input))))

(defun eval-zero-or-more (rule input rules &optional acc)
  (format *standard-output* "eval zero or more ~A ~A~%" rule acc)
  (multiple-value-bind (result new-input)
      (eval-statements nil rule input rules)
    (if result
        (eval-zero-or-more rule new-input rules (cons result acc))
        (values (or (reverse acc) t) input))))

(defun eval-or (statements input rules)
  (if statements
      (multiple-value-bind (result new-input)
          (eval-statement (first statements) input rules)
        (format *standard-output* "eval or ret ~A ~A~%" result new-input)
        (if result
            (values result new-input)
            (eval-or (rest statements) input rules)))
      (values nil input)))

(defun eval-call (expr input rules)
  (let ((op (first expr))
        (args (rest expr)))
    (format *standard-output* "eval call ~A ~A ~A ~A~%" expr (safe-elt input 0) op args)
    (cond
      ((eq op 'quote) (eval-exact-match (first args) input))
      ((eq op 'range) (eval-range (first args) (second args) input))
      ((eq op 'or) (eval-or args input rules))
      ((eq op '+) (eval-one-or-more args input rules))
      ((eq op '*) (eval-one-or-more args input rules))
      ((eq op '?) (eval-zero-or-one args input rules))
      (t (error 'statement-error :statement expr))))
  )

(defun find-rule (rule rules)
  (if (eq rule (first (first rules)))
      (first rules)
      (find-rule rule (rest rules))))

(defun eval-statement (statement input rules)
  (cond
    ((eq statement 'nil) (eval-match-eos input))
    ((symbolp statement) (eval-rule (find-rule statement rules) input rules))
    ((atom statement) (eval-exact-match statement input))
    ((listp statement) (eval-call statement input rules))
    (t (error 'statement-error :statement statement)))
  )

(defun action-arglist (n &optional acc)
  (if (listp n)
      (action-arglist (length n) acc)
      (if (numberp n)
          (if (>= n 0)
              (action-arglist (- n 1) (cons (intern (format nil "$~A" n)) acc))
              acc)
          (action-arglist 1 acc))
      ))

(defun eval-action (action acc)
  (if (atom acc)
      (eval-action action (list acc))
      (progn
                                        ;(setf acc (reverse acc))
        (format *standard-output* "eval action ~A ~A~%" action acc)
        (cond
          ((symbolp action) (apply action (cons acc acc)))
          ((listp action)
           (let ((arglist (action-arglist acc)))
             (apply (eval `(lambda ,arglist
                             (declare (ignorable ,@arglist))
                             ,action))
                    (cons acc acc))))
          (t (error 'statement-error :statement action))))
      ))

(defun call-action-stack (action-stack &optional acc)
  (if action-stack
      (let* ((action (first action-stack))
             (form (first action))
             (input (rest action)))
        (call-action-stack (rest action-stack) (cons (eval-action form (if (listp input)
                                                                           (reverse input)
                                                                           input)) acc)))
      acc))

(defun eval-statements (name statements input rules &optional acc action-stack)
  (format *standard-output* "eval ~A ~A~%" name statements)
  (let ((statement (first statements)))
    (cond
      ((eq '> statement)
       (eval-statements name
                        (rest (rest statements))
                        input
                        rules
                        (eval-action (first (rest statements)) (reverse acc))
                        action-stack))
      ((eq '@ statement)
       (eval-statements name
                        (rest (rest statements))
                        input
                        rules
                        nil
                        (cons (cons (first (rest statements)) acc) action-stack)))
      ((eq statement nil)
       (format *standard-output* "eval action-stack ~A~%" acc) ;; acc usused args?
       (values (if action-stack
                   (call-action-stack action-stack)
                   acc)
               input))
      (t
       (multiple-value-bind (result new-input)
           (eval-statement statement input rules)
         (if result
             (eval-statements name (rest statements) new-input rules (cons result acc) action-stack)
             (values nil new-input))
         )))))

(defun eval-rule (rule input rules)
  (eval-statements (first rule) (rest rule) input rules))

(defun eval-rules (rules input)
  (eval-rule (first rules) input rules))


(defmacro make-parser (&rest rules)
  ;; rule := name statement
  ;; statement := expression >action?
  ;; expression := rule-name | value | #\( op-call #\)
  ;; op-call := op (* statement)
  ;; op := (or * + ? not and or range)
  ;; value := (or number character)
  ;;
  ;; each rule becomes a function that reads the next byte/event, tries the statement, and calls the action when the statement matches
  )

(defun test-range ()
  (let ((test-parser '((root (+ digit))
                       (digit (range #\0 #\9) @(print acc)))))
    (eval-rules test-parser "123 45 6")))

(defun print-args (&rest args)
  (print args)
  (first (or (second args) (first args))))

(defun test-actions ()
  (let ((test-parser '((root (+ digit >(print-args 'digit $0) @(print 'digit-done $0)) >(print-args 'root $0) @(print-args 'root-done $0))
                       (digit (range #\0 #\9) >(- (char-code $1) (char-code #\0)) @(print-args 'digit-done $0)))))
    (eval-rules test-parser "123 45 6")))

(defun test-list (&optional (lst '(a b a b a c)))
  (let ((test-parser '((root (or ab ac) >(print-args 'root $0) (or nil root))
                       (ab 'a 'b >(print $0) @(print $0))
                       (ac 'a 'c >(print $0) @(print $0)))))
    (eval-rules test-parser lst)))

(defun test-string (&optional (str "ababac"))
  (let ((test-parser '((root (or ab ac) >(print 'root) (or nil root))
                       (ab #\a >(print 'on-ab) @(print 'act-ab) #\b >(print $0) @(print $0))
                       (ac #\a >(print 'on-ac) @(print 'act-ac) #\c >(print $0) @(print $0)))))
    (eval-rules test-parser str)))

(defvar *tokens* '())
(defun push-token (a)
  (setf *tokens* (cons a *tokens*))
  a)
(defun digit-value (c) (- (char-code c) (char-code #\0)))
(defun digit-list-to-number (lst &optional neg (n 0) (base 10))
  (if lst
      (digit-list-to-number (rest lst) neg (+ (* n base) (first lst)))
      (if neg
          (- n)
          n)))

(defun test-or (&optional (str "ababc12ac  ab  -13"))
  (setf *tokens* '())
  (let* ((test-parser '((top root (or nil top))
                        (root (or abc ab ac number space) >(print-args 'root $1))
                        (space (or #\space #\newline))
                        (abc #\a #\b #\c @(push-token 'abc) @(print-args 'ab $0 $1))
                        (ab #\a #\b >(print-args 'on-ab $0 $1) @(push-token 'ab) @(print-args 'ab $0 $1))
                        (ac #\a #\c @(push-token 'ac) @(print-args 'ac $0 $1))
                        (number (? #\-)
                         (+ (range #\0 #\9) >(digit-value $1))
                         @(push-token (digit-list-to-number $2 (not (eq $1 T))))))))
    (values (eval-rules test-parser str)
            (reverse *tokens*))))


;; todo parse per token: (read-token) => type, value
;; todo switch to input and output strings/stacks
