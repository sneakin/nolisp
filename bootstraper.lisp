(in-package :repl)

(defun bootstrap-eval (exp)
  (cond
    ((eq exp nil) nil)
    ((atom exp) nil)
    ((eq (first exp) 'repl-defstruct) (macroexpand exp))
    ((eq (first exp) 'cl-user::repl-defstruct) (macroexpand exp))
    ))

(defun bootstrap-stream (stream &optional expansions)
  (handler-case
      (let ((exp (read stream)))
        (if exp
            (bootstrap-stream stream (cons (bootstrap-eval exp) expansions))
            expansions))
    (end-of-file (e)
      expansions))
  )

(defun bootstrap-process (path)
  (with-open-file (stream path :direction :input)
    (bootstrap-stream stream)))

(defun bootstrap-write (path exps)
  (with-open-file (stream path :direction :output
                          :if-exists :supersede)
    (mapcar #'(lambda (exp)
                (write exp :stream stream))
            (remove-if #'null exps))))

(defun bootstrap-gen (source-files)
  (if source-files
      (let ((src (first source-files)))
        (format *standard-output* "Processing ~A -> ~A~%" (first src) (second src))
        (bootstrap-write (second src) (bootstrap-process (first src)))
        (bootstrap-gen (rest source-files)))))

(bootstrap-gen '(("compiler/package.lisp" "bootstrap/package.nl")
                 ("compiler/byte-buffer.lisp" "bootstrap/byte-buffer.nl")
                 ("compiler/symbol-index.lisp" "bootstrap/symbol-index.nl")
                 ("compiler/imports-list.lisp" "bootstrap/imports-list.nl")))
