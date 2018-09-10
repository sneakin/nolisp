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

(defun token-seq-read-list (seq output temp strings strings-end &optional (terminator (char-code #\))) temp-start)
  (let ((special-sym (symbol-intern 'special strings strings-end))
        (eos-sym (symbol-intern 'eos strings strings-end)))
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
        ;; end of list
        ((and (eq kind special-sym) (eq value terminator))
         (let* ((new-temp (ptr-write-long 0 temp))
                (new-output (ptr-copy temp-start output (- new-temp temp-start))))
           (values output rest-seq new-output (or temp-start temp))))
        ;; end of stream, before terminating
        ((or (eq kind eos-sym) (eq kind 0))
         (error 'malformed-error :offset seq))
        ;; add the element and move on
        (t (let ((new-head (ptr-cons output kind value)))
             (token-seq-read-list rest-seq new-head (ptr-write-long output temp) strings strings-end terminator (or temp-start temp))))
        ))))

(defun token-seq-read (seq output temp strings strings-end)
  (let ((special-sym (symbol-intern 'special strings strings-end)))
    (multiple-value-bind (kind value rest-seq)
        (token-seq-next seq)
      (format *standard-output* ";; ~A ~A ~A ~A~%" seq kind value output)
      (cond
        ((and (eq kind special-sym) (eq value (char-code #\()))
         (token-seq-read-list rest-seq output temp strings strings-end (char-code #\))))
        (t (values output rest-seq (ptr-cons output kind value) temp)))))
  )

(defun type-id? (ptr)
  (let ((sym (symbol-string ptr)))
    (or (string-equal sym "integer")
        (string-equal sym "float")
        (string-equal sym "symbol")
        (string-equal sym "string"))))

(defun atom? (lst)
  (if (> lst 0)
      (let ((type-id (ptr-head lst)))
        (type-id? type-id))
      t))

(defun symbol? (lst)
  (if (> lst 0)
      (string-equal "symbol" (symbol-string (ptr-head lst)))
      nil))

(defun string? (lst)
  (if (> lst 0)
      (string-equal "string" (symbol-string (ptr-head lst)))
      nil))

(defun integer? (lst)
  (if (> lst 0)
      (string-equal "integer" (symbol-string (ptr-head lst)))
      nil))

(defun float? (lst)
  (if (> lst 0)
      (string-equal "float" (symbol-string (ptr-head lst)))
      nil))

(defun number? (lst)
  (or (integer? lst)
      (float? lst)))

(defun print-list-inner (lst)
  (if (null? (ptr-head lst))
      (format *standard-output* ")~%")
      (progn (format *standard-output* "~A ~A ~A ~A ~A ~A~%" (atom? (ptr-head lst)) lst (ptr-head lst) (ptr-head (ptr-rest lst))
                     (if (symbol? (ptr-head lst))
                         (symbol-string (ptr-head (ptr-head lst)))
                         (ptr-head (ptr-head lst)))
                     (if (symbol? (ptr-head lst))
                         (symbol-string (ptr-head (ptr-rest (ptr-head lst))))
                         (ptr-head (ptr-rest (ptr-head lst)))))
             (print-list-inner (ptr-rest lst)))
      ))

(defun print-list (lst)
  (format *standard-output* "~A: ( " lst)
  (print-list-inner lst))

(defun print-cell (cell)
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
     (format *standard-output* "(~A . ~A)" (ptr-head cell) (ptr-tail cell)))))

(defun print-seq (seq &optional (first t))
  (if first (format *standard-output* "("))
  (if (null? (ptr-head seq))
      (format *standard-output* ")")
      (progn (if (not first) (format *standard-output* " "))
             (if (atom? (ptr-head seq))
                 (print-cell (ptr-head seq))
                 (print-seq (ptr-head seq)))
             (print-seq (ptr-rest seq) nil))))


(defun test-token-seq ()
  (ptr-write-string "((hello what 987) -2.3 123 (world 456) \"what\")" 0)
  (multiple-value-bind (offset strings-end output-end)
      (token-sequence 0 1024 2048)
    (format *standard-output* "Input: ~A" (ptr-read-string 0))
    (format *standard-output* "~%Seq:")
    (print-seq (print (token-seq-read 2048 3072 4096 1024 strings-end)))
    (format *standard-output* "~%List:")
    (print-list (token-seq-read 2048 3072 4096 1024 strings-end))
    (format *standard-output* "~%Raw seq:")
    (print (ptr-read-array 3072 128))))
