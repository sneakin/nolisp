#-:repl
(in-package :repl)

#-:repl (require "defenum")

#-:repl
(defenum LOGGER-LEVEL
  (QUIET 0)
  (FATAL 1)
  (WARNING 2)
  (INFO 3)
  (VERBOSE 4)
  (DEBUG 5))

#+:repl (require "bootstrap/logging")

(defvar *LOGGER-LEVEL* LOGGER-LEVEL-INFO)

#-:repl (defconstant LOGGER-LEVEL-KEYWORDS '(:quiet :fatal :warning :info :verbose :debug))

#-:repl (define-condition bad-logger-level (simple-error)
  ((level :initform 0 :initarg :level)))

#-:repl (defvar *LOGGER-STREAM* *error-output*)

#-:repl (defun logger-init (level &optional (stream *error-output*))
  (if level
      (setq *LOGGER-LEVEL* (logger-level-number level)))
  (setq *LOGGER-STREAM* stream))

#+:repl (defun logger-init (level &optional stream)
  (if level
      (setq *LOGGER-LEVEL* (logger-level-number level))))

(defun logger-for? (level)
  (<= (logger-level-number level) *LOGGER-LEVEL*))

#-:repl
(defun logger (level &rest args)
  (if (logger-for? level)
      (apply #'format *logger-stream* args)))

#+:repl
(require "runtime/bc/io/console")
#+:repl
(require "runtime/io")

#+:soon
(defun logger (level msg &optional a b c d e f g)
  (if (logger-for? level)
      (format-lambda console-write-byte/1 msg a b c d e f g)))

#+:repl
(defun logger (level msg &optional a b c d e f g)
  (format-lambda console-write-byte/1 msg a b c d e f g))

