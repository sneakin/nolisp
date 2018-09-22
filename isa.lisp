;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

(require "type-sizes")
(require "conditions")

(defconstant *SIZEOF_INST* *SIZEOF_SHORT*)

(define-condition disassembler-error (repl-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unknown disassembly error: ~A~%" condition))))

(defun concat-symbol (a b)
  (intern (concatenate 'string (symbol-name a) (symbol-name b))))

(defun nth-alpha-symbol (n)
  (intern (coerce (list (code-char (+ (char-code #\A) n))) 'string)))

(defun alpha-seq (n &optional acc (c 0))
  (if (> n 0)
      (alpha-seq (- n 1)
                 (cons (nth-alpha-symbol (+ c (- n 1)))
                       acc)
                 c)
      acc))

(defun nibble-padding (n &optional (c 0))
  (alpha-seq (- 4 n) nil c))

(defun pad-op-nibbles (nibbles &optional (c 0))
  (concatenate 'list nibbles (nibble-padding (length nibbles) c)))

(defun gen-isa-op-encoder (op nibbles &optional data)
  `(,op (make-short ,(pad-op-nibbles nibbles))))

(defun gen-isa-make-ops (prefix ops &optional acc)
  (if ops
      (gen-isa-make-ops prefix (rest ops) (cons (apply #'gen-isa-op-encoder (first ops))
                                         acc))
      `(,(concat-symbol prefix '-make-op) (op &optional a b c)
         ,acc)
      ))

(defun gen-isa-op-nibble-tests (nibbles &optional acc (n 0))
  (if nibbles
      (gen-isa-op-nibble-tests (rest nibbles)
                               (cons `(eq ,(nth-alpha-symbol n) ,(first nibbles)) acc)
                               (+ 1 n))
      acc))

(defun gen-isa-op-disassembler (op nibbles &optional data)
  `((and ,@(gen-isa-op-nibble-tests nibbles))
    (list ,op ,@(nibble-padding (length nibbles) (length nibbles)))))

(defun gen-isa-make-dis (prefix ops &optional acc)
  (if ops
      (gen-isa-make-dis prefix
                        (rest ops)
                        (cons (gen-isa-op-disassembler (first (first ops))
                                                       (second (first ops)))
                              acc))
      `(,(concat-symbol prefix '-op-disassemble) (op)
         (multiple-value-bind (a b c d)
             (nibble-short (elt op 0)
                           (elt op 1))
           ;; (format *standard-output* "~A ~A ~A ~A~%" a b c d)
           (cond ,@acc
                 (t (cons 'unknown-op op))
                 ; (t (error 'unknown-op-error :op op))
                 )))))
 

(defun gen-isa-op-data-size-case (op nibbles &optional data)
  `((eq ,op (first op))
    ,(type-size data)))


(defun gen-isa-op-data-size (prefix ops &optional acc)
  (if ops
      (gen-isa-op-data-size prefix
                            (rest ops)
                            (cons (gen-isa-op-data-size-case (first (first ops))
                                                             (second (first ops))
                                                             (third (first ops)))
                                  acc))
      `(,(concat-symbol prefix '-op-data-size) (op)
         (cond ,@acc
               (t 0)))))


(defun gen-isa-op-data-type-case (op nibbles &optional data)
  `((eq ,op (first op))
    ,data))


(defun gen-isa-op-data-type (prefix ops &optional acc)
  (if ops
      (gen-isa-op-data-type prefix
                            (rest ops)
                            (cons (gen-isa-op-data-type-case (first (first ops))
                                                             (second (first ops))
                                                             (third (first ops)))
                                  acc))
      `(,(concat-symbol prefix '-op-data-type) (op)
         (cond ,@acc))
      ))

(defun nibble-short (hi lo)
  (values (logand (ash hi 0) #xF)
          (logand (ash hi -4) #xF)
          (logand (ash lo 0) #xF)
          (logand (ash lo -4) #xF)))

(defun seq-read-long (seq offset)
  (logior (ash (elt seq (+ offset 0)) 0)
          (ash (elt seq (+ offset 1)) 8)
          (ash (elt seq (+ offset 2)) 16)
          (ash (elt seq (+ offset 3)) 24)))

(defun seq-read-short (seq offset)
  (logior (ash (elt seq (+ offset 0)) 0)
          (ash (elt seq (+ offset 1)) 8)))

(defun seq-read-byte (seq offset)
  (elt seq (+ offset 0)))

(defun seq-read-float (seq offset)
  (let ((bits (seq-read-long seq offset)))
    (sb-kernel:make-single-float bits)))

(defun read-op-data (data-type seq offset)
  (case data-type
    (:long (seq-read-long seq offset))
    (:ulong (seq-read-long seq offset))
    (:short (seq-read-short seq offset))
    (:ushort (seq-read-short seq offset))
    (:byte (seq-read-byte seq offset))
    (:ubyte (seq-read-byte seq offset))
    (:float (seq-read-float seq offset))
    (otherwise (error 'unknown-data-type-error :type data-type))))

(defun disassemble-op (op-fn data-fn data-type-fn seq offset)
  (let* ((op (funcall op-fn (subseq seq offset (+ offset *SIZEOF_INST*))))
         (data-size (funcall data-fn op))
         (data-type (funcall data-type-fn op)))
    (if (> data-size 0)
        (values (list offset op (read-op-data data-type seq (+ offset *SIZEOF_INST*)))
                (+ offset *SIZEOF_INST* data-size))
        (values (list offset op) (+ offset *SIZEOF_INST*)))))

(defun dis-repl-code (op-fn data-fn data-type-fn seq &optional (offset 0) acc)
  (if (< offset (length seq))
      (multiple-value-bind (op next-offset)
          (disassemble-op op-fn data-fn data-type-fn seq offset)
        (dis-repl-code op-fn data-fn data-type-fn seq next-offset (cons op acc)))
      (reverse acc)))

(defun seq-read-string (ptr offset)
  (let ((str-end (position 0 ptr :start offset)))
    (if str-end
        (coerce (mapcar #'code-char (coerce (subseq ptr offset str-end) 'list)) 'string))))

(defun listify-strings (seq offset ending &optional acc)
  (if (< offset ending)
      (let ((str (seq-read-string seq offset)))
        (if str
            (listify-strings seq
                             (+ 1 offset (length str))
                             ending
                             (cons (list offset str) acc))
            acc))
      acc))

(defun listify-longs (seq offset ending &optional acc)
  (if (and (< offset (- ending *SIZEOF_LONG*))
           (< offset (- (length seq) *SIZEOF_LONG*)))
      (let ((i (seq-read-long seq offset)))
        (if (not (eq i nil))
            (listify-longs seq
                           (+ offset *SIZEOF_LONG*)
                           ending
                           (cons (list offset i) acc))
            acc))
      acc))

(defun dis-repl (op-fn data-fn data-type-fn seq &optional (offset 0))
  (let* ((indexes (- (length seq) (* 3 *SIZEOF_LONG*)))
         (code-segment (seq-read-long seq indexes))
         (strings (seq-read-long seq (+ *SIZEOF_LONG* indexes)))
         (toplevel (seq-read-long seq (+ *SIZEOF_LONG* *SIZEOF_LONG* indexes))))
    (values (dis-repl-code op-fn data-fn data-type-fn (subseq seq 0 strings) offset)
            code-segment
            (listify-strings seq strings toplevel)
            (mapcar #'(lambda (entry)
                        (cons (first entry)
                              (+ (second entry)
                                 strings)))
                    (listify-longs seq toplevel indexes)))))

(defmacro define-isa (prefix &rest ops)
  `(labels (,(gen-isa-make-ops prefix ops)
            ,(gen-isa-make-dis prefix ops)
             ,(gen-isa-op-data-size prefix ops)
             ,(gen-isa-op-data-type prefix ops))
          (defun ,(concat-symbol prefix '-disassemble) (seq &optional (offset 0))
            (dis-repl #',(concat-symbol prefix '-op-disassemble)
                      #',(concat-symbol prefix '-op-data-size)
                      #',(concat-symbol prefix '-op-data-type)
                      seq
                      offset)
            )))
