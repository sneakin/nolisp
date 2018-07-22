;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "type-sizes")

(in-package :repl)

(defun env-push-alloc (bytes env)
  (format *standard-output* ";; Binding ~A bytes~%" bytes)
  (+ env (align-bytes bytes) *REGISTER-SIZE*))

(defun env-push-alloc-binding (name bytes env)
  (env-push-binding name (env-push-alloc env bytes)))

(defun env-pop-alloc (bytes env)
  (format *standard-output* ";; Unbinding ~A bytes~%" bytes)
  (- (env-pop-bindings env 1) (align-bytes bytes) *REGISTER-SIZE*))

(defun env-symbol-position (sym env-start env &optional (n 0))
  (if (> env env-start)
      (if (eq (ptr-read-long (- env *REGISTER-SIZE*)) sym)
          n
          (env-symbol-position sym env-start (- env *REGISTER-SIZE*) (+ 1 n)))
      nil))

(defun env-data-position (sym env-start env)
  (let ((pos (env-symbol-position sym env-start env)))
    (if pos
        (- (/ (- env env-start) *REGISTER-SIZE*) pos 1))))

(defun env-stack-position (sym env-start env)
  (env-symbol-position sym env-start env))

(defun env-define (name env-start env)
  (let ((pos (env-symbol-position name env-start env)))
    (if pos
        env
        (env-push-binding name env))))
