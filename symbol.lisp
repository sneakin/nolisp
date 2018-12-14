;;; -*- mode: Lisp; coding: utf-8-unix -*-

(in-package :repl)

#+:repl
(defun package-string-segment-data (obj))
#+:repl
(defun package-string-segment-end (obj))
#+:repl
(defun set-package-string-segment-position (obj n))

#+:repl
(defun symbolp (ptr)
  t)

(defun keyword? (sym)
  (eq (ptr-read-byte sym)
      (char-code #\:)))

#+:repl
(defun keywordp (sym)
  (keyword? sym))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string (if (keyword? symbol-offset)
                           (+ 1 symbol-offset)
                           symbol-offset))))

(defun symbol-id (symbol-offset &optional (segment (package-string-segment-data *COMPILER*)))
  (ptr-find-string-equal (ptr-read-string symbol-offset) segment symbol-offset))

#+:repl
(defun symbol-name (sym)
  (symbol-string sym))

#+:repl
(defun symbolp (ptr)
  (> ptr (package-string-segment-data *COMPILER*)))

(defun symbol-has-arity? (sym)
  (if (eq (ptr-read-byte sym) (char-code #\/))
      t
      (if (eq (ptr-read-byte sym) 0)
          nil
          (symbol-has-arity? (+ sym 1)))))

#+:repl
(defun symbol-concat (a b &optional c d e (offset (package-string-segment-end *COMPILER*)) orig-offset)
  (if b
      (symbol-concat b c d e nil (ptr-copy a offset (string-length a)) (or orig-offset offset))
      (values orig-offset (ptr-write-byte 0 offset))
      ))


(defun symbol-intern (str segment-start segment-end)
  (if (symbolp str)
      (if (keywordp str)
          (setq str (symbol-concat ":" (symbol-name str)))
          (setq str (symbol-name str))))
  (let* ((off (ptr-write-string str segment-end))
         (id (symbol-id segment-end segment-start)))
    (if id
        (values id segment-end)
        (values segment-end off))))

#+:repl
(defun intern (sym &optional (package *COMPILER*))
  (symbol-intern sym
                 (package-string-segment-data package)
                 (package-string-segment-end package)))

#-:repl
(defun symbol-concat-pkg-inner (parts &optional (acc ""))
  (if parts
      (let ((p (first parts)))
        (symbol-concat-pkg-inner (rest parts)
                                 (concatenate 'string acc
                                              (if (symbolp p)
                                                  (symbol-name p)
                                                  (if (numberp p)
                                                      (ptr-read-string p)
                                                      p)))))
      (string-upcase acc)))

#-:repl
(defun symbol-concat-pkg (pkg &rest args)
  (intern (symbol-concat-pkg-inner args) pkg))

#+:repl
(defun symbol-concat-pkg (pkg &optional a b c d e)
  (symbol-concat a b c d e))

#+:sbcl
(defun symbol-concat (&rest parts)
  (apply 'symbol-concat-pkg *package* parts))

#+:sbcl
(defun intern-keyword (str)
  (if (symbolp str)
      str
      (intern (string-upcase str) "KEYWORD")))

#-:sbcl
(defun intern-keyword (str)
  (with-allocation (buffer 128)
    (ptr-write-string str buffer)
    (intern (string-upcase buffer))))
