(in-package :repl-windows)

(require "windows/exe-writer-2")
(require "windows/import-data")

(defun write-code-segment (exe stream package)
  (write-pe-segment exe stream ".text"
                    (repl::package-code-segment-buffer package)
                    (repl::package-code-segment-position package)
                    (logior PE-SECTION-CNT-CODE
                            PE-SECTION-MEM-READ
                            PE-SECTION-MEM-EXECUTE)))

(defun write-string-segment (exe stream package)
  (write-pe-segment exe stream ".strings"
                    (repl::package-string-segment-data package)
                    (repl::package-string-segment-position package)
                    (logior PE-SECTION-CNT-INITIALIZED-DATA
                            PE-SECTION-MEM-READ)))

(defun write-symbol-segment (exe stream package)
  (write-pe-segment exe stream ".symbols"
                    (repl::package-symbols-buffer package)
                    (repl::package-symbols-size package)
                    (logior PE-SECTION-CNT-INITIALIZED-DATA
                            PE-SECTION-MEM-READ)))

(defun write-bss-segment (exe stream package)
  (write-pe-segment exe stream ".bss"
                    nil
                    (repl::package-symbols-size package)
                    (logior PE-SECTION-CNT-UNINITIALIZED-DATA
                            PE-SECTION-MEM-READ
                            PE-SECTION-MEM-WRITE)))

(defun write-idata-segment (exe stream package pe32+)
    ;; write import data
  (multiple-value-bind (offset size padded-size)
      (write-idata-segment-data exe stream package pe32+)
    ;; update segment header
    (mark-pe-segment exe stream ".idata" offset padded-size size (logior PE-SECTION-CNT-INITIALIZED-DATA
                                                                         PE-SECTION-MEM-WRITE
                                                                         PE-SECTION-MEM-READ))))

(defun write-misc-segment (exe stream &optional (msg "Made using NoLisp."))
  (repl::with-allocation (buffer 1024)
    (repl::ptr-write-string msg buffer)
    (write-pe-segment exe stream ".misc"
                      buffer
                      (length msg)
                      (logior PE-SECTION-CNT-INITIALIZED-DATA
                              PE-SECTION-MEM-READ))))


(defun write-package (path package &optional (pe32+ 1))
  (with-pe-file path 6 pe32+
                (lambda (exe f)
                  (set-pe-writer-state-code-segment exe (write-code-segment exe f package))
                  (write-string-segment exe f package)
                  (write-symbol-segment exe f package)
                  (write-bss-segment exe f package)
                  (write-idata-segment exe f package pe32+)
                  (write-misc-segment exe f))))
