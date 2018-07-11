(in-package :repl)

(require "memory")

#-:sbcl
(defun index-of (needle haystack)
  (let ((ptr (pointer-of needle haystack)))
    (if ptr
        (- ptr needle)
      nil)))

#+:sbcl
(defun index-of (needle haystack &optional (n 0))
  (when (< n (length haystack))
    (if (eq needle (char-code (aref haystack n)))
        n
      (index-of needle haystack (+ 1 n)))))
