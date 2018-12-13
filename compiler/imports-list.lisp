;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "env")
(require "type-sizes")

(in-package :repl)

#+:repl (require "bootstrap/imports-list")
#-:repl (require "runtime/defstruct")

#-:repl
(repl-defstruct import-entry
                ((lib :type :pointer)
                 (fun-name :type :pointer)))

(defun import-entry-fun-name-string (entry)
  (ptr-read-string (import-entry-fun-name entry)))

#-:repl
(repl-defstruct imports-list
                ((max-size)
                 (buffer)
                 (next-offset)))

(defun imports-list-init (index max-size buffer)
  (set-imports-list-max-size index max-size)
  (set-imports-list-buffer index buffer)
  (set-imports-list-next-offset index buffer))

(defun imports-list-find (index lib symbol &optional (offset (imports-list-buffer index)))
  (if (eq offset (imports-list-next-offset index))
      nil
      (if (and (eq lib (import-entry-lib offset))
               (eq symbol (import-entry-fun-name offset)))
          offset
          (imports-list-find index lib symbol (+ offset (import-entry-size))))))

(defun imports-list-define (index lib symbol)
  (let ((i (imports-list-find index lib symbol)))
    (if (not i)
        (set-imports-list-next-offset index
                                      (ptr-write-ptr symbol (ptr-write-ptr lib (imports-list-next-offset index)))))
    index))

(defun imports-list-position (index)
  (- (imports-list-next-offset index)
     (imports-list-buffer index)))

(defun imports-list-count (index)
  (/ (imports-list-position index)
     (import-entry-size)))

(defun imports-list-get (index n)
  (let ((offset (* n (import-entry-size))))
    (if (< offset (imports-list-position index))
        (+ (imports-list-buffer index)
           offset))))

