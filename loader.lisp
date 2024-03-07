;;; -*- mode: Lisp; coding: utf-8-unix -*-
;;; Functions to load and reload REPL including module path resolution and loading for SBCL.

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

(defun repl-source-path (mod-name &optional (ext "lisp"))
  (make-pathname :name (if (symbolp mod-name)
                           (string-downcase (symbol-name mod-name))
                           mod-name)
                 :type ext))

;; todo need to also search multiple file directories
(defconstant *repl-extensions* '("lisp" "nl"))
(defvar *load-path* (list (pathname-directory ".") (pathname-directory *load-truename*)))

(defun repl-resolve-path (mod-name &optional (extensions *repl-extensions*))
  (if extensions
      (let ((file-name (repl-source-path mod-name (first extensions))))
        (if (probe-file file-name)
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
  (pushnew #'repl-module-loader
    #+:sbcl *module-provider-functions*
    #+:ecl ext:*module-provider-functions*)
  (if (and reload (find "REPL" *modules* :test #'string=))
      (setf *modules* nil))
  (require :repl))

(defun repl-reload ()
  (load "loader.lisp")
  (repl-load t))

#+:ecl
(eval-when (:load-toplevel)
  (require '#:package-locks)
  (si:fset 'sys-require #'require)
  (ext:without-package-locks
    (defun require (mod &rest paths) (format *error-output* "Not requiring ~A~%" mod))))

(eval-when (:load-toplevel :execute)
  (repl-load))
;;(repl-load)

(provide :repl-loader)
