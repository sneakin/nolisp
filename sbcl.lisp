;;; -*- mode: Lisp; coding: utf-8-unix; eval: (slime-eval-buffer) -*-
;;; Functions to load and reload REPL including module path resolution and loading for SBCL.

(defvar *TOP-MODULE* "nolisp")
(defconstant *repl-extensions* '("lisp" "nl"))
(defvar *load-path* (append (directory ".") (directory "./src")))

(define-condition repl-load-error (simple-error)
  ((module :initarg :module :initform nil)
   (path :initarg :path :initform nil))
  (:report (lambda (condition stream)
             (format stream "Load Error on ~A: ~A~%"
                     (slot-value condition 'module)
                     (slot-value condition 'path)))))

(define-condition repl-module-not-found-error (repl-load-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Module ~A not found: ~A.~%"
                     (slot-value condition 'module)
                     (slot-value condition 'path)))))

(defun repl-source-path (mod-name &key (type "lisp") directory)
  (make-pathname :name (if (symbolp mod-name)
                           (string-downcase (symbol-name mod-name))
                           mod-name)
                 :directory (pathname-directory directory)
                 :type type))

(defun repl-resolve-path-dir (name &key type (dirs *load-path*))
  (if dirs
      (let ((filename (repl-source-path name
                                        :type type
                                        :directory (first dirs))))
        (if (probe-file filename)
            filename
            (repl-resolve-path-dir name :type type :dirs (rest dirs))))))

(defun repl-resolve-path (mod-name &optional (extensions *repl-extensions*))
  (if extensions
      (let ((file-name (repl-resolve-path-dir mod-name :type (first extensions))))
        (if file-name
            file-name
            (repl-resolve-path mod-name (rest extensions))))
      nil))

(defun repl-module-loader (mod-name)
  (let* ((file-name (repl-resolve-path mod-name)))
    (unless file-name (error 'repl-module-not-found-error :module mod-name :path file-name))
    (format *error-output* "Requiring ~A ~A~%" mod-name file-name)
    (if (load file-name)
        (provide mod-name)
        (error 'repl-load-error :module mod-name :path file-name))))

(defun repl-load (&optional reload)
  (unless (find #'repl-module-loader *module-provider-functions*)
    (push #'repl-module-loader *module-provider-functions*))
  (if (and reload (find *TOP-MODULE* *modules* :test #'string=))
      (setf *modules* nil))
  (require *TOP-MODULE*))

(defun repl-reload ()
  (load "sbcl.lisp")
  (repl-load t))

(eval-when (:load-toplevel :execute)
  (repl-load))
