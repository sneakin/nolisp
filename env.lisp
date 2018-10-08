;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "memory")
(require "type-sizes")
(require "symbol")

(in-package :repl)

(defun env-push-binding (name env)
  "Adds NAME to ENV and returns the new ENV stack address."
  (format *standard-output*
          ";; Binding ~A ~A to ~A~%"
          (if (> name 0)
              (ptr-read-string name)
              name)
          name
          env)
  (ptr-write-long name env)
  (+ *REGISTER-SIZE* env))

(defun env-pop-bindings (env num)
  "Removes NUM bindings from ENV returning the new ENV stack address."
  (format *standard-output* ";; Unbinding ~A slots~%" num)
  (if (> num 0)
      (env-pop-bindings (- env *REGISTER-SIZE*) (- num 1))
      env))

(defun env-push-alloc (bytes env)
  "Adds BYTES worth of space to the ENV stack returning the new address."
  (format *standard-output* ";; Binding ~A bytes~%" bytes)
  (+ env (align-bytes bytes) *REGISTER-SIZE*))

(defun env-push-alloc-binding (name bytes env)
  "Adds a named binding of BYTES to ENV."
  (env-push-binding name (env-push-alloc env bytes)))

(defun env-pop-alloc (bytes env)
  "Removes a binding of allocated bytes."
  (format *standard-output* ";; Unbinding ~A bytes~%" bytes)
  (- (env-pop-bindings env 1) (align-bytes bytes) *REGISTER-SIZE*))

(defun env-symbol-position (sym env-start env &optional (n 0))
  "Returns the binding slot index of SYM in the environment defined by ENV-START and ENV."
  (if (>= env env-start)
      (if (eq (ptr-read-ulong (- env *REGISTER-SIZE*)) sym)
          n
          (env-symbol-position sym env-start (- env *REGISTER-SIZE*) (+ 1 n)))
      nil))

(defun env-data-position (sym env-start env)
  "Returns the index into the data segment of SYM."
  (let ((pos (env-symbol-position sym env-start env)))
    (if pos
        (- (/ (- env env-start) *REGISTER-SIZE*) pos 1))))

#+:repl (defun symbol-string (symbol-offset))

(defun env-function-position (func-name env-start env)
  "Returns the index of FUNC-NAME in the code segment."
  (let* ((idx (env-data-position func-name env-start env)))
    (format *standard-output* ";; resolving ~A to ~A~%" (symbol-string func-name) idx)
    (if idx
        idx
        (env-data-position func-name env-start env))))

(defun env-stack-position (sym env-start env)
  "Returns the index of SYM for the environment stored on the stack."
  (env-symbol-position sym env-start env))

(defun env-define (name env-start env)
  "Defines a new named binding in an environment."
  (let ((pos (env-symbol-position name env-start env)))
    (if pos
        env
        (env-push-binding name env))))

(defun env-copy-binding (env src dest)
  (ptr-write-long (ptr-read-long (+ env (* src *REGISTER-SIZE*)))
                  (+ env (* dest *REGISTER-SIZE*)))
  env)

(defun env-move-bindings (env num-move dest-offset &optional (n num-move) (post-env (- env (* *REGISTER-SIZE* num-move))))
  (if (> n 0)
      (env-move-bindings (env-copy-binding env (- n 1) (+ dest-offset (- num-move n)))
                         num-move
                         dest-offset
                         (- n 1)
                         post-env)
      (values (- env (* (- dest-offset num-move) *REGISTER-SIZE*))
              post-env)))

(defun env-size (env-start env)
  (/ (- env env-start) *REGISTER-SIZE*))

(defun env-name (env-binding)
  (let ((ptr (ptr-read-ulong env-binding)))
    (if (and ptr (not (zero? ptr)))
        (ptr-read-string ptr))))

(defun env-dump (env-start env &optional (stream *standard-output*) (n 0))
  (if (= n 0)
      (format stream "~%Env: ~A to ~A~%" env-start env))
  (if (< env-start env)
      (progn
        (format stream "~A ~A = ~A~%" (ptr-read-ulong env-start) (env-name env-start) n)
        (env-dump (+ env-start *REGISTER-SIZE*) env stream (+ n 1)))
      (format stream "~A bindings~%~%" n)))
