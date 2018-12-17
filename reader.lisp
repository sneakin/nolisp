;;; -*- mode: Lisp; coding: utf-8-unix -*-
#+:sbcl
(require "conditions")

(require "memory")
(require "symbol")
(require "sequence")
(require "logging")

#+:repl (require "runtime/math/float")
(require "runtime/convertors")

#-:repl
(in-package :repl)

;;; Base of the read numbers
(defvar *SYMBOL-SPECIALS* ".,:-~!@$%^&*_=+\\/?<>|#")
(defvar *LIST-INITIATORS* "([{")
(defvar *LIST-TERMINATORS* ")]}")
(defvar *SPECIALS* "()[]\"'`")
#+:sbcl
(defvar *SPACES* (format nil "~c~c~c~c" #\space #\newline #\tab #\return))
#+:repl
(defvar *SPACES* " \t\n\r")

(defun symbol-special? (c)
  (not (eq -1 (index-of c *SYMBOL-SPECIALS*))))

(defun special? (c)
  (not (eq -1 (index-of c *SPECIALS*))))

(defun list-initiator? (c)
  (not (eq -1 (index-of c *LIST-INITIATORS*))))

(defun list-initiator-for (c)
  (let ((i (index-of c *LIST-TERMINATORS*)))
    (if (eq i -1)
        nil
        (string-aref *LIST-INITIATORS* i))))

(defun list-terminator? (c)
  (not (eq -1 (index-of c *LIST-TERMINATORS*))))

;; todo signify if C is not an initiator
(defun list-terminator-for (c)
  (let ((i (index-of c *LIST-INITIATORS*)))
    (if (eq i -1)
        nil
        (string-aref *LIST-TERMINATORS* i))))

(defun list-terminator-code-for (c)
  (char-code (list-terminator-for c)))

(defun newline? (c)
  (or (eq c (char-code #\newline)) (eq c (char-code #\return))))

(defun space? (c)
  (not (eq -1 (index-of c *SPACES*))))

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

(defun read-symbol (str output sym-tab)
  (multiple-value-bind (kind value offset token-offset)
      (read-symbol-inner str output)
    (let ((sym-id (if sym-tab (symbol-id value sym-tab))))
      (if sym-id
          (values kind sym-id offset output)
          (values kind value offset token-offset)))))

(defun digit-value (c)
  (cond
    ((digit? c) (- c (char-code #\0)))
    ((lower-alpha? c) (+ 10 (- c (char-code #\a))))
    ((upper-alpha? c) (+ 10 (- c (char-code #\A))))
    (t 0)))

(defun read-signed-number (str acc base token-offset &optional (allow-decimal t))
  (let* ((c (ptr-read-byte str))
         (n (digit-value c)))
    (cond
      ((and (or (digit? c) (and (alpha? c) (> base 10)))
            (< n base))
       (read-signed-number (+ 1 str) (+ (* base acc) n) base token-offset allow-decimal))
      ((and (alpha? c) (>= n base))
       (error 'invalid-character-error :offset str :value c))
      ((and allow-decimal (eq c (char-code #\.)))
       (multiple-value-bind (kind dec-value next-str next-token-offset)
           (read-signed-number (+ 1 str) 0 base token-offset nil)
         (values 'float
                 (make-float-decimal acc dec-value nil base (- next-str (+ 1 str)))
                 next-str next-token-offset)))
      (t (values 'integer acc str token-offset)))))

(defun read-negative-number (str acc base token-offset sym-tab)
  (let ((c (ptr-read-byte str)))
    (if (or (digit? c) (and (alpha? c) (> base 10)))
        (multiple-value-bind (kind value offset token-offset)
            (read-signed-number str acc base token-offset)
          (cond
            ((eq kind 'float) (values kind (--float value) offset token-offset))
            ((eq kind 'integer) (values kind (- value) offset token-offset))
            (t (error 'invalid-character-error :offset str))))
        (read-symbol (- str 1) token-offset sym-tab))))

(defun read-plus (str token-offset sym-tab)
  (let ((c (ptr-read-byte (+ 1 str))))
    (cond
      ((digit? c) (read-signed-number (+ 1 str) 0 *INPUT-BASE* token-offset))
      (t (read-symbol str token-offset sym-tab)))))

(defun read-number (str acc base token-offset sym-tab)
  (let ((c (ptr-read-byte str)))
    (cond
      ((eq c (char-code #\-))
       (read-negative-number (+ str 1) 0 base token-offset sym-tab))
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
    ((eq c (char-code #\0)) #\0)
    (t (error 'invalid-escape-error :char c))))

(defun read-number-pair (str base)
  (let ((a (ptr-read-ubyte str))
        (b (ptr-read-ubyte (+ 1 str))))
    (+ (* base (digit-value a))
               (digit-value b))))

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
       (let ((cc (ptr-read-byte (+ 1 str))))
         (cond
           ;; todo \d### \b \v
           ;; read \xNN escapes
           ((eq cc (char-code #\x))
            (let ((n (read-number-pair (+ 2 str) 16)))                
              (ptr-write-byte n output)
              (read-string (+ 4 str) (+ output 1) terminator (or output-start output))))
           ;; basic single character escapes: \C
           (t (ptr-write-byte (char-code (unescape-char cc)) output)
              (read-string (+ str 2) (+ output 1) terminator (or output-start output))))))
      (t
       (ptr-write-byte c output)
       (read-string (+ str 1) (+ output 1) terminator (or output-start output))))))

(defun character-by-name (char-sym)
  (cond
    ((string-equal char-sym "Nul") #\Nul)
    ((string-equal char-sym "Soh") #\Soh)
    ((string-equal char-sym "Stx") #\Stx)
    ((string-equal char-sym "Etx") #\Etx)
    ((string-equal char-sym "Eot") #\Eot)
    ((string-equal char-sym "Enq") #\Enq)
    ((string-equal char-sym "Ack") #\Ack)
    ((string-equal char-sym "Bel") #\Bel)
    ((string-equal char-sym "Backspace") #\Backspace)
    ((string-equal char-sym "Tab") #\Tab)
    ((string-equal char-sym "Newline") #\Newline)
    ((string-equal char-sym "Vt") #\Vt)
    ((string-equal char-sym "Page") #\Page)
    ((string-equal char-sym "Return") #\Return)
    ((string-equal char-sym "So") #\So)
    ((string-equal char-sym "Si") #\Si)
    ((string-equal char-sym "Dle") #\Dle)
    ((string-equal char-sym "Dc1") #\Dc1)
    ((string-equal char-sym "Dc2") #\Dc2)
    ((string-equal char-sym "Dc3") #\Dc3)
    ((string-equal char-sym "Dc4") #\Dc4)
    ((string-equal char-sym "Nak") #\Nak)
    ((string-equal char-sym "Syn") #\Syn)
    ((string-equal char-sym "Etb") #\Etb)
    ((string-equal char-sym "Can") #\Can)
    ((string-equal char-sym "Em") #\Em)
    ((string-equal char-sym "Sub") #\Sub)
    ((string-equal char-sym "Esc") #\Esc)
    ((string-equal char-sym "Fs") #\Fs)
    ((string-equal char-sym "Gs") #\Gs)
    ((string-equal char-sym "Rs") #\Rs)
    ((string-equal char-sym "Us") #\Us)
    ((string-equal char-sym " ") #\space)
    ((string-equal char-sym "space") #\space)
    ((string-equal char-sym "rubout") #\rubout)
    ((eq (length char-sym) 1) (string-aref char-sym 0))
    (t (error 'invalid-character-error :value char-sym))
    ))

(defun read-character-symbol (str token-offset &optional (starting nil))
  (let ((c (ptr-read-byte str)))
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
    (cond
      (char (if (and (symbol-char? char) (symbol-char? c))
                (read-character-symbol (- str 1) token-offset)
                (values 'character char str token-offset)))
      ((not char) (read-character (+ 1 str) token-offset c))
      (t (error 'invalid-character-error :offset str :char c)))))

(defun read-reader-macro (str token-offset sym-tab)
  (let ((c (ptr-read-byte str)))
    (cond
      ;; #\char
      ((eq c (char-code #\\)) (read-character (+ 1 str) token-offset))
      ;; #xHEX
      ((or (eq c (char-code #\x)) (eq c (char-code #\X))) (read-number (+ 1 str) 0 16 token-offset sym-tab))
      ;; #+expr
      ((eq c (char-code #\+)) (values 'condition t str token-offset))
      ;; #-expr
      ((eq c (char-code #\-)) (values 'condition nil str token-offset))
      ;; anything else
      ;; todo lookup in table
      ((eq c (char-code #\:)) (read-symbol str token-offset sym-tab))
      ((eq c (char-code #\')) (read-symbol (+ 1 str) token-offset sym-tab))
      (t (error 'invalid-character-error :offset str :char c))
      ))
  )

(defun read-token (str token-offset &optional sym-tab)
  (let ((c (ptr-read-ubyte str)))
    (cond
      ((space? c) (read-token (+ 1 str) token-offset sym-tab))
      ((digit? c) (read-signed-number str 0 *INPUT-BASE* token-offset))
      ((eq c (char-code #\+)) (read-plus str token-offset sym-tab))
      ((eq c (char-code #\-)) (read-negative-number (+ 1 str) 0 *INPUT-BASE* token-offset sym-tab))
      ((eq c (char-code #\#)) (read-reader-macro (+ 1 str) token-offset sym-tab))
      ((or (alpha? c)
           (symbol-special? c)) (read-symbol str token-offset sym-tab))
      ((eq c (char-code #\")) (read-string (+ 1 str) token-offset #\"))
      ((eq c (char-code #\;)) (read-token (read-comment (+ 1 str)) token-offset sym-tab))
      ((special? c) (values 'special c (+ 1 str) token-offset))
      ((zero? c) (values 'eos nil (+ 1 str) token-offset))
      (t (values 'unknown c (+ 1 str) token-offset)))))

(defun scan-list (offset token-offset &optional (initiator (char-code #\()) (terminator (char-code #\))) (depth 0))
  (multiple-value-bind (kind value offset new-token-offset)
      (read-token offset token-offset)
    (cond
      ((eq kind 'symbol)
       (scan-list offset new-token-offset initiator terminator depth))
      ((and (eq kind 'special) (eq value initiator))
       (scan-list offset token-offset initiator terminator (+ 1 depth))) ; go down
      ((and (eq kind 'special) (eq value terminator))
       (if (<= depth 1)
           (values offset token-offset) ; done
           (scan-list offset token-offset initiator terminator (- depth 1)))) ; move back up
      (t (scan-list offset token-offset initiator terminator depth))))) ; keep reading
