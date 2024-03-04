;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "type-sizes")
(require "memory")
(require "string")

(in-package :repl)

(define-condition slot-error (simple-error)
  ((slot :initform nil :initarg :slot)
   (struct :initform nil :initarg :struct)))

(defun slot-def-assoc (key fields)
  (if fields
      (if (eq (first fields) key)
          (first (rest fields))
          (slot-def-assoc key (rest (rest fields))))))

(defun slot-def-name (slot)
  (first slot))

(defun slot-def-init-form (slot)
  (slot-def-assoc :initform (rest slot)))

(defun slot-def-align (slot)
  (slot-def-assoc :align (rest slot)))

(defun slot-def-size (slot)
  (slot-def-assoc :size (rest slot)))

(defun slot-def-type (slot)
  (or (slot-def-assoc :type (rest slot)) :long))

(defun struct-size (type)
  (funcall (symbol-concat (if (listp type)
                              (second type)
                              type)
                          "-size")))

(defun slot-def-type-size (slot)
  (let ((type (or (slot-def-type slot) :long)))
    (if (type-atom? type)
        (type-size type)
        (struct-size type))))


(defun gen-struct-offset (n offset slot)
  `((eq slot ',(slot-def-name slot)) ,offset))

(defun gen-struct-offsets (struct-name slots &optional (n 0) (offset 0) acc)
  (if slots
      (let* ((slot (first slots))
             (alignment (slot-def-align slot))
             (offset (if alignment
                         (align-bytes offset alignment)
                         offset))
             (slot-size (* (or (slot-def-size (first slots))
                               1)
                           (slot-def-type-size (first slots)))))
        (gen-struct-offsets struct-name (rest slots)
                            (+ n 1)
                            (+ offset slot-size)
                            (cons (gen-struct-offset n offset (first slots))
                                  acc)))
      `((defun ,(symbol-concat struct-name "-slot-offset") (slot)
         (cond
           ,@acc
           (t (error 'slot-error :slot slot :struct ',struct-name))))
        (defun ,(symbol-concat struct-name "-size") ()
          ,offset))))

(defun slot-def-writer (struct-name slot)
  (let ((type (slot-def-type slot)))
    (if (type-atom? type)
        (symbol-concat-pkg (find-package :repl) "ptr-write-" type)
        (symbol-concat "copy-" (slot-def-type slot)))))

(defun slot-def-reader (struct-name slot)
  (let ((type (slot-def-type slot)))
    (if (type-atom? type)
        (symbol-concat-pkg (find-package :repl) "ptr-read-" type))))

(defun gen-struct-array-accessor (struct-name slot)
  (let* ((slot-name (slot-def-name slot))
         (ref-method (symbol-concat struct-name "-" slot-name "-ref")))
    `((defun ,ref-method (obj &optional (n 0))
        (+ obj (,(symbol-concat struct-name "-slot-offset") ',slot-name)
           (* n ,(slot-def-type-size slot))))
      ,(if (slot-def-reader struct-name slot)
           `(defun ,(symbol-concat struct-name "-" slot-name) (obj &optional (n 0))
              (,(slot-def-reader struct-name slot) (,ref-method obj n))))
      (defun ,(symbol-concat "set-" struct-name "-" slot-name) (obj value &optional (n 0))
        ,(if (type-atom? (slot-def-type slot))
             `(,(slot-def-writer struct-name slot) value (,ref-method obj n))
             `(if value
                  (ptr-copy value (,ref-method obj n) ,(* (or (slot-def-size slot) 1) (slot-def-type-size slot)))
                  (ptr-zero (,ref-method obj n)
                            ,(* (or (slot-def-size slot) 1) (slot-def-type-size slot)))))
        obj))))

(defun gen-struct-accessor (struct-name slot)
  (let* ((slot-name (slot-def-name slot))
         (ref-method (symbol-concat struct-name "-" slot-name "-ref")))
    `((defun ,ref-method (obj)
        (+ obj (,(symbol-concat struct-name "-slot-offset") ',slot-name)))
      (defun ,(symbol-concat struct-name "-" slot-name) (obj)
        ,(if (slot-def-reader struct-name slot)
             `(,(slot-def-reader struct-name slot) (,ref-method obj))
             `(,ref-method obj)))
      (defun ,(symbol-concat "set-" struct-name "-" slot-name) (obj value)
        ,(if (type-atom? (slot-def-type slot))
             `(,(slot-def-writer struct-name slot) value (,ref-method obj))
             `(if value
                  (ptr-copy value (,ref-method obj) ,(* (or (slot-def-size slot) 1) (slot-def-type-size slot)))
                  (ptr-zero (,ref-method obj)
                      ,(* (or (slot-def-size slot) 1) (slot-def-type-size slot))))
             )
        obj))))

(defun gen-struct-accessors (struct-name slots &optional acc)
  (if slots
      (gen-struct-accessors struct-name
                            (rest slots)
                            (append (if (slot-def-size (first slots))
                                        (gen-struct-array-accessor struct-name (first slots))
                                        (gen-struct-accessor struct-name (first slots)))
                                    acc))
      acc))

(defun gen-struct-init-arglist (slots &optional arglist)
  (if slots
      (let ((slot (first slots)))
        (gen-struct-init-arglist (rest slots)
                                 (if (not (slot-def-size slot))
                                     (cons (let ((init-form (slot-def-init-form slot)))
                                             (if init-form
                                                 `(,(slot-def-name slot) ,init-form)
                                                 (slot-def-name slot)))
                                           arglist)
                                     arglist)))
      (reverse arglist)))

(defun gen-struct-init-setters (struct-name slots &optional acc)
  (if slots
      (gen-struct-init-setters struct-name
                               (rest slots)
                               (cons (if (not (slot-def-size (first slots)))
                                         `(,(symbol-concat "set-" struct-name "-" (slot-def-name (first slots))) obj ,(slot-def-name (first slots)))
                                         `(ptr-zero obj ,(slot-def-type-size (first slots))))
                                     acc))
      (reverse acc)))

(defun gen-struct-init (struct-name slots)
  (let ((init-func (symbol-concat struct-name "-init")))
    `(defun ,init-func (obj &optional ,@(gen-struct-init-arglist slots))
       ,@(gen-struct-init-setters struct-name slots)
       obj)))

(defun gen-struct-copier (name)
  `(defun ,(symbol-concat "copy-" name) (src dest)
     (ptr-copy src dest (,(symbol-concat name "-size")))
     dest))

(defun structure-struct-size ()
  (* *SIZEOF_LONG* 3))

(defun structure-struct-init (obj &optional name slots init-func)
  obj)

(defmacro repl-defstruct (name slots)
  `(progn
     ,@(gen-struct-offsets name slots)
     ,@(gen-struct-accessors name slots)
     ,(gen-struct-copier name)
     ,(gen-struct-init name slots)
     ;(defconstant ,name ',(symbol-concat name "-init"))
     ))
