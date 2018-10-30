;;; -*- mode: Lisp; coding: utf-8-unix -*-
#+:sbcl
(require "conditions")

(require "memory")
(require "symbol")
(require "sequence")

(in-package :repl)

;;; Base of the read numbers
(defvar *NUMBER-BASE* 10)
(defvar *SYMBOL-SPECIALS* ".,:-~!@$%^&*_=+\\/?<>|#")
(defvar *LIST-INITIATORS* "([{")
(defvar *LIST-TERMINATORS* ")]}")
(defvar *SPECIALS* "()[]\"'`")
#+:sbcl
(defvar *SPACES* (format nil "~c~c~c~c" #\space #\newline #\tab #\return))
#+:repl
(defvar *SPACES* " 	
")

(defun symbol-special? (c)
  (not (eq nil (index-of c *SYMBOL-SPECIALS*))))

(defun special? (c)
  (not (eq nil (index-of c *SPECIALS*))))

(defun list-initiator? (c)
  (not (eq nil (index-of c *LIST-INITIATORS*))))

(defun list-initiator-for (c)
  (string-aref *LIST-INITIATORS* (index-of c *LIST-TERMINATORS*)))

(defun list-terminator? (c)
  (not (eq nil (index-of c *LIST-TERMINATORS*))))

(defun list-terminator-for (c)
  (string-aref *LIST-TERMINATORS* (index-of c *LIST-INITIATORS*)))

(defun list-terminator-code-for (c)
  (char-code (list-terminator-for c)))

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
    (let ((sym-id (symbol-id value)))
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

(defun read-signed-number (str acc base token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
      ((or (digit? c) (and (alpha? c) (> base 10)))
       (read-signed-number (+ 1 str) (+ (* base acc) (digit-value c)) base token-offset))
      ((and (alpha? c) (<= base 10))
       (error 'invalid-character-error :offset str :value c))
      ((eq c (char-code #\.)) (read-decimal (+ 1 str) acc base token-offset 1))
      (t (values 'integer acc str token-offset)))))

(defun read-negative-number (str acc base token-offset)
  (let ((c (ptr-read-byte str)))
    (if (digit? c)
        (multiple-value-bind (kind value offset token-offset)
            (read-signed-number str acc base token-offset)
          (values kind (- value) offset token-offset))
        (read-symbol (- str 1) token-offset))))

(defun read-plus (str token-offset)
  (let ((c (ptr-read-byte (+ 1 str))))
    (cond
      ((digit? c) (read-signed-number (+ 1 str) 0 *NUMBER-BASE* token-offset))
      (t (read-symbol str token-offset)))))

(defun read-number (str acc base token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
      ((eq c (char-code #\-))
       (read-negative-number (+ str 1) 0 base token-offset))
      ((eq c (char-code #\+))
       (read-signed-number (+ str 1) 0 base token-offset))
      (t (read-signed-number str acc base token-offset)))))

(defun read-comment (str)
  (if (newline? (ptr-read-byte str))
      (+ 1 str)
      (read-comment (+ 1 str))))

(defun unescape-char (c)
  (cond
    ((eq c (char-code #\")) #\")
    ((eq c (char-code #\')) #\')
    ((eq c (char-code #\n)) #\newline)
    ((eq c (char-code #\r)) #\return)
    ((eq c (char-code #\t)) #\tab)
    ((eq c (char-code #\\)) #\\)
    ((eq c (char-code #\/)) #\/)
    (t (error 'invalid-escape-error :char c))))

(defun read-string (str output &optional (terminator #\") output-start)
  (let ((c (ptr-read-byte str))
        (terminator (if (numberp terminator)
                        terminator
                        (char-code terminator))))
    (cond
      ((eq c terminator)
       (ptr-write-byte 0 output)
       (values 'string (or output-start output) (+ 1 str) (+ 1 output)))
      ((eq c (char-code #\\))
       (ptr-write-byte (char-code (unescape-char (ptr-read-byte (+ 1 str)))) output)
       (read-string (+ str 2) (+ output 1) terminator (or output-start output)))
      (t
       (ptr-write-byte c output)
       (read-string (+ str 1) (+ output 1) terminator (or output-start output))))))

(defun character-by-name (char-sym)
  (format *standard-output* "Char by name ~A~%" char-sym)
  (cond
    ((string-equal char-sym "space") #\space)
    ((string-equal char-sym "newline") #\newline)
    ((string-equal char-sym "linefeed") #\linefeed)
    ((string-equal char-sym "return") #\return)
    ((string-equal char-sym "tab") #\tab)
    ((string-equal char-sym "backspace") #\backspace)
    ((string-equal char-sym "page") #\page)
    ((string-equal char-sym "rubout") #\rubout)
    ((eq (length char-sym) 1) (string-aref char-sym 0))
    (t (error 'invalid-character-error :value char-sym))
    ))

(defun read-character-symbol (str token-offset &optional (starting nil))
  (let ((c (ptr-read-byte str)))
    (format *standard-output* "read-char-symbol ~A ~A ~A ~A ~A~%" str c (code-char c) (space? c) (null c))
    (cond
      ((or (space? c) (null c) (not (symbol-char? c)))
       (ptr-write-byte 0 token-offset)
       (values 'character
               (char-code (character-by-name (ptr-read-string (or starting token-offset))))
               str
               (+ 1 token-offset)))
      (t
       (ptr-write-byte c token-offset)
       (read-character-symbol (+ 1 str) (+ 1 token-offset) (or starting token-offset))))))

(defun read-character (str token-offset &optional char)
  (let ((c (ptr-read-byte str)))
    (format *standard-output* "char ~A ~A ~%" c (if char (code-char char)))
    (cond
      (char (if (and (symbol-char? char) (symbol-char? c))
                (read-character-symbol (- str 1) token-offset)
                (values 'character char str token-offset)))
      ((not char) (read-character (+ 1 str) token-offset c))
      (t (error 'invalid-character-error :offset str :char c)))))

(defun read-reader-macro (str token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
      ;; #\char
      ((eq c (char-code #\\)) (read-character (+ 1 str) token-offset))
      ;; #xHEX
      ((or (eq c (char-code #\x)) (eq c (char-code #\X))) (read-number (+ 1 str) 0 16 token-offset))
      ;; #+expr
      ((eq c (char-code #\+)) (values 'condition t str token-offset))
      ;; #-expr
      ((eq c (char-code #\-)) (values 'condition nil str token-offset))
      ;; anything else
      ;; todo lookup in table
      (t (error 'invalid-character-error :offset str :char c))
      ))
  )

(defun read-token (str token-offset)
  (let ((c (ptr-read-byte str)))
    (cond
      ((space? c) (read-token (+ 1 str) token-offset))
      ((digit? c) (read-signed-number str 0 *NUMBER-BASE* token-offset))
      ((eq c (char-code #\+)) (read-plus str token-offset))
      ((eq c (char-code #\-)) (read-negative-number (+ 1 str) 0 *NUMBER-BASE* token-offset))
      ((eq c (char-code #\#)) (read-reader-macro (+ 1 str) token-offset))
      ((or (alpha? c)
           (symbol-special? c)) (read-symbol str token-offset))
      ((eq c (char-code #\")) (read-string (+ 1 str) token-offset #\"))
      ((eq c (char-code #\;)) (read-token (read-comment (+ 1 str)) token-offset))
      ((special? c) (values 'special c (+ 1 str) token-offset))
      ((zero? c) (values 'eos nil (+ 1 str) token-offset))
      (t (values 'unknown nil (+ 1 str) token-offset)))))

(defun scan-list (offset token-offset &optional (initiator (char-code #\()) (terminator (char-code #\))) (depth 0))
  (multiple-value-bind (kind value offset new-token-offset)
      (read-token offset token-offset)
    ;; (format *standard-output* "scan ~A: ~A ~A~%" depth kind (if (eq kind 'symbol) (symbol-string value) value))
    (cond
      ((eq kind 'symbol)
       (scan-list offset new-token-offset initiator terminator depth))
      ((and (eq kind 'special) (eq value initiator))
       (scan-list offset token-offset initiator terminator (+ 1 depth))) ; go down
      ((and (eq kind 'special) (eq value terminator))
       (if (<= depth 1)
           (progn (format *standard-output* "  done~%")
                  (values offset token-offset)) ; done
           (scan-list offset token-offset initiator terminator (- depth 1)))) ; move back up
      (t (scan-list offset token-offset initiator terminator depth))))) ; keep reading
