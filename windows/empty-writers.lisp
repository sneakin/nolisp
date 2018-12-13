(in-package :repl-windows)

;;; Test functions

(defun fake-code-segment-data (out-ptr count)
  (repl::ptr-set out-ptr count #x90) ; nops
  (let ((offset (repl-amd64::emit-ops-64 out-ptr
                                         ;;(:int3)
                                         (push 123)
                                         (pop :eax)
                                         (ret))))
    (- offset out-ptr)))

(defun write-fake-code-segment (exe f)
  (repl::with-allocation (data 1024)
    (write-pe-segment exe f ".text" data
                      (fake-code-segment-data data 1024)
                      (logior PE-SECTION-CNT-CODE
                              PE-SECTION-MEM-EXECUTE
                              PE-SECTION-MEM-READ))))

(defun write-fake-string-segment (exe f)
  (repl::with-allocation (data 1024)
    (let* ((end-offset (repl::ptr-write-string "Hello" (repl::ptr-write-string "World" data)))
           (data-len (- end-offset data)))
      (write-pe-segment exe f ".data" data data-len (logior PE-SECTION-CNT-INITIALIZED-DATA
                                                            PE-SECTION-MEM-WRITE
                                                            PE-SECTION-MEM-READ)))))

(defun write-empty-pe-file (path)
  (with-pe-file path 3 nil
                (lambda (exe f)
                  (multiple-value-bind (cs code-size)
                      (write-fake-code-segment exe f)
                    (write-fake-string-segment exe f)
                    (write-pe-segment exe f ".bss" nil 12 (logior PE-SECTION-CNT-UNINITIALIZED-DATA
                                                                  PE-SECTION-MEM-READ
                                                                  PE-SECTION-MEM-WRITE))
                    (set-pe-writer-state-code-segment exe cs)))))

(defun write-empty-pe32+-file (path)
  (with-pe-file path 3 1
                (lambda (exe f)
                  (multiple-value-bind (cs code-size)
                      (write-fake-code-segment exe f)
                    (write-fake-string-segment exe f)
                    (write-pe-segment exe f ".bss" nil 12 (logior PE-SECTION-CNT-UNINITIALIZED-DATA
                                                                  PE-SECTION-MEM-READ
                                                                  PE-SECTION-MEM-WRITE))
                    (set-pe-writer-state-code-segment exe cs)))))
