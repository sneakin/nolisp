;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(defun token-sequence (input string-offset output &optional (token-segment string-offset))
  (setf *token-segment* token-segment)
  (multiple-value-bind (kind value offset new-string-offset)
      (read-token input string-offset)
    (multiple-value-bind (token-start token-end)
        (symbol-intern kind token-segment new-string-offset)
      (let ((new-output (ptr-cons output token-start value)))
        (if (or (eq kind 'eos) (eq kind 0))
            (values offset token-end new-output)
            (token-sequence offset token-end new-output token-segment))))))

(defun token-seq-next (seq)
  (let ((offset (+ seq *SIZEOF_LONG*)))
    (values (ptr-read-ulong seq) (ptr-read-ulong offset) (+ offset *SIZEOF_LONG*))))

(defun token-seq-read-terminate (rest-seq output temp-start temp strings strings-end tail-item)
  (let* ((new-temp (ptr-write-long (if tail-item tail-item 0) temp))
         (new-output (ptr-copy temp-start output (- new-temp temp-start))))
    (values output rest-seq new-output (or temp-start temp))))

(defun token-seq-read-list (seq output temp strings strings-end &optional (terminator (char-code #\))) temp-start tail-item)
  (let ((special-sym (symbol-intern 'special strings strings-end))
        (eos-sym (symbol-intern 'eos strings strings-end))
        (symbol-sym (symbol-intern 'symbol strings strings-end)))
    (multiple-value-bind (kind value rest-seq)
        (token-seq-next seq)
      (cond
        ;; sub-list
        ((and (eq kind special-sym) (eq value (char-code #\()))
         (multiple-value-bind (output new-seq new-output new-temp)
             (token-seq-read-list rest-seq output temp strings strings-end (list-terminator-code-for value))
           ;; read the next element
           (token-seq-read-list new-seq
                                new-output
                                (ptr-write-long output temp)
                                strings
                                strings-end
                                terminator
                                (or temp-start temp))))
        ;; tail
        ((and (eq kind symbol-sym) (string-equal (symbol-string value) "."))
         ;; read tail item
         (multiple-value-bind (output tail-seq new-output new-temp)
             (token-seq-read rest-seq output temp strings strings-end)
           ;; read terminator
           (multiple-value-bind (kind value done-seq)
               (token-seq-next tail-seq)
             ;; nothing but a terminator
             (unless (and (eq kind special-sym) (eq value terminator))
               (error 'malformed-error :offset tail-seq))
             ;; end of list
             (token-seq-read-terminate done-seq new-output temp-start temp strings strings-end output))))
        ;; end of list
        ((and (eq kind special-sym) (eq value terminator))
         (token-seq-read-terminate rest-seq output temp-start temp strings strings-end 0))
        ;; end of stream, before terminating
        ((or (eq kind eos-sym) (eq kind 0))
         (error 'malformed-error :offset seq))
        ;; add the element and move on
        (t (let ((new-head (ptr-cons output kind value)))
             (token-seq-read-list rest-seq new-head (ptr-write-long output temp) strings strings-end terminator (or temp-start temp))))))))

(defun token-seq-read (seq output temp strings strings-end)
  (let ((special-sym (symbol-intern 'special strings strings-end)))
    (multiple-value-bind (kind value rest-seq)
        (token-seq-next seq)
      (format *standard-output* ";; ~A ~A ~A ~A~%" seq kind value output)
      (cond
        ((and (eq kind special-sym) (eq value (char-code #\()))
         (token-seq-read-list rest-seq output temp strings strings-end (char-code #\))))
        (t (values output rest-seq (ptr-cons output kind value) temp))))))

(defun type-id? (ptr)
  (let ((sym (symbol-string ptr)))
    (or (string-equal sym "integer")
        (string-equal sym "float")
        (string-equal sym "symbol")
        (string-equal sym "string"))))

(defun null? (cell)
  (or (eq cell nil)
      (and (zero? (ptr-head cell))
           (zero? (ptr-tail cell)))))

(defun atom? (lst)
  (if (null? lst)
      t
      (let ((type-id (ptr-head lst)))
        (type-id? type-id))))

(defun symbol? (lst)
  (if (null? lst)      
      nil
      (string-equal "symbol" (symbol-string (ptr-head lst)))))

(defun string? (lst)
  (if (null? lst)
      nil
      (string-equal "string" (symbol-string (ptr-head lst)))))

(defun integer? (lst)
  (if (null? lst)
      nil
      (string-equal "integer" (symbol-string (ptr-head lst)))))

(defun float? (lst)
  (if (null? lst)
      nil
      (string-equal "float" (symbol-string (ptr-head lst)))))

(defun number? (lst)
  (or (integer? lst)
      (float? lst)))

(defun print-list-inner (lst)
  (progn (format *standard-output* "~A ~A ~A ~A ~A ~A~%"
                 (atom? (ptr-head lst))
                 lst
                 (ptr-head lst)
                 (ptr-head (ptr-rest lst))
                 (if (symbol? (ptr-head lst))
                     (symbol-string (ptr-head (ptr-head lst)))
                     (ptr-head (ptr-head lst)))
                 (if (symbol? (ptr-head lst))
                     (symbol-string (ptr-head (ptr-rest (ptr-head lst))))
                     (ptr-head (ptr-rest (ptr-head lst)))))
         (if (null? (ptr-rest lst))
             (format *standard-output* ")~%")
             (print-list-inner (ptr-rest lst)))))

(defun print-list (lst)
  (format *standard-output* "~A: ( " lst)
  (print-list-inner lst))

(defun print-cell (cell &optional (first t))
  (cond
    ((symbol? cell)
     (format *standard-output* "symbol:~A" (symbol-string (ptr-tail cell))))
    ((string? cell)
     (format *standard-output* "string:\"~A\"" (symbol-string (ptr-tail cell))))
    ((integer? cell)
     (format *standard-output* "integer:~A" (ptr-tail cell)))
    ((float? cell)
     (format *standard-output* "float:~A" (ptr-tail-float cell)))
    ((null? cell)
     (format *standard-output* "nil"))
    (t
     (if first
         (format *standard-output* "(")
         (if (null? (ptr-rest (ptr-rest cell)))
             (format *standard-output* " . ")
             (format *standard-output* " ")))
     (print-cell (ptr-head cell))
     (if (null? (ptr-tail cell))
         (format *standard-output* ")")
         (print-cell (ptr-rest cell) nil)))))

(defun test-token-seq (&optional (input "((hello what 987) -2.3 123 (world 456) \"what\")"))
  (ptr-write-string input 0)
    (multiple-value-bind (offset strings-end output-end)
        (token-sequence 0 1024 2048)
      (assert (eq offset (+ 1 (length input))))
      (format *standard-output* "Input: ~A" (ptr-read-string 0))
      (format *standard-output* "~%Cell:")
      (print-cell (print (token-seq-read 2048 3072 4096 1024 strings-end)))
      (format *standard-output* "~%List:")
      (print-list (token-seq-read 2048 3072 4096 1024 strings-end))
      (format *standard-output* "~%Raw seq:")
      (print (ptr-read-array 3072 128))))
