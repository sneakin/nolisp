(in-package :repl)

(defun symbol-id (symbol-offset &optional (segment *TOKEN-SEGMENT*))
  (ptr-find-string-equal (ptr-read-string symbol-offset) segment symbol-offset))

(defun symbol-string (symbol-offset)
  (if symbol-offset
      (ptr-read-string symbol-offset)))

(defun symbol-intern (str start ending)
  (let* ((off (ptr-write-string str ending))
         (id (symbol-id off start)))
    (if id id off)))

