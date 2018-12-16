;;; -*- mode: Lisp; coding: utf-8-unix -*-

(require "reader")
(require "runtime/bc/io")
(require "runtime/io")

(defun vt100-escape (esc)
  (output-dev-write-byte #\esc)
  (output-dev-write esc))

(defun vt100-reset-color ()
  (vt100-escape "[0m"))

(defun vt100-reset ()
  (vt100-escape "c")
  (vt100-reset-color))

(defun vt100-yellow ()
  (vt100-escape "[1;33m"))

(defun vt100-gray ()
  (vt100-escape "[2;37m"))

(defun vt100-red ()
  (vt100-escape "[31m"))

(defun vt100-green ()
  (vt100-escape "[1;32m"))

(defun vt100-white ()
  (vt100-escape "[1;37m"))

(defun print-tokens (str len tokens)
  (if (> len 0)
      (multiple-value-bind (kind value offset token-offset)
          (read-token str tokens)
        (format nil "Kind: ~s\tValue: ~d\tOffset: ~d~%" (symbol-name kind) value offset)
        (cond
          ((eq kind 'symbol) (format nil "\t\"~s\"~%" (symbol-name value)))
          ((eq kind 'string) (format nil "\t~s~%" value))
          ((eq kind 'integer) (format nil "\t~d\t~x~%" value value))
          ((eq kind 'float) (format nil "\t~f~%" value))
          ((eq kind 'EOS) nil))
        (if (not (eq kind 'EOS))
            (print-tokens offset (- len (- offset str)) tokens)))))

(defun looper (line-buffer max-line token-buffer)
  (vt100-yellow)
  (output-dev-write "> ")
  (vt100-white)
  (ptr-zero line-buffer max-line)
  (if (input-dev-eos)
      (progn
        (vt100-red)
        (format nil "Goodbye~%")
        (vt100-reset-color))
      (let ((n (input-dev-readline line-buffer max-line)))
        (if n
            (progn
              (vt100-gray)
              (format nil "Read: \"~s\"~%" line-buffer)
              (vt100-reset-color)
              (print-tokens line-buffer n token-buffer)))
        (looper line-buffer max-line token-buffer))))

(defun main ()
  (io-init)
  (vt100-reset)
  (vt100-green)
  (format nil "Hello!~%")
  (with-allocation (line-buffer 1024)
    (with-allocation (token-buffer 1024)
      (looper line-buffer 1024 token-buffer))))

(main)
