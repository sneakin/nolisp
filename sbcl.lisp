;;; -*- mode: Lisp; coding: utf-8-unix -*-

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

(defun repl-source-path (mod-name)
  (make-pathname :name (if (symbolp mod-name)
                           (string-downcase (symbol-name mod-name))
                         mod-name)
                 :type "lisp"))

(defun repl-module-loader (mod-name)
  (let* ((file-name (repl-source-path mod-name)))
    (unless file-name (error 'repl-module-not-found-error :module mod-name :path file-name))
    (format *error-output* "Requiring ~A ~A~%" mod-name file-name)
    (if (load file-name)
        (provide mod-name)
      (error 'repl-load-error :module mod-name :path file-name)))
  )

(defun repl-load (&optional reload)
  (unless (find #'repl-module-loader *module-provider-functions*)
    (push #'repl-module-loader *module-provider-functions*))
  (if (and reload (find "REPL" *modules* :test #'string=))
      (setf *modules* nil))
  (require :repl)
  )

(defun repl-reload ()
  (load "sbcl.lisp")
  (repl-load t))

(eval-when (:load-toplevel :execute)
           (repl-load))

