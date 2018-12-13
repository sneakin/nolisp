(in-package :repl-windows)

(defun write-null-hint-name-entry (stream)
  (repl::with-allocation (entry (pe-hint-name-size))
    (set-pe-hint-name-hint entry 0)
    (write-sequence (repl::ptr-read-array entry repl::*SIZEOF_SHORT*)
                    stream)))

(defun write-idata-hint-name-entry (stream import)
  (repl::with-allocation (entry (pe-hint-name-size))
    (set-pe-hint-name-hint entry 1)
    (set-pe-hint-name-name entry (repl::import-entry-fun-name-string import))
    (write-sequence (repl::ptr-read-array entry
                                          (pe-hint-name-true-size entry))
                    stream)))

(defun write-idata-hint-names (stream package idata-segment import-segment-offset out-buffer &optional (n 0) (cur-out out-buffer))
  ;; fixme can be a flat list, need to collect file offsets for import-lookups
  ;; each DLL has a list of symbols to import
  (if (< n (repl::package-imports-count package))
      (let* ((offset (file-position stream))
             (rva (+ idata-segment
                     (- offset import-segment-offset))))
        ;; write the pe-hint-name
        (write-idata-hint-name-entry stream (repl::package-get-import package n))
        ;; next
        (write-idata-hint-names stream package idata-segment import-segment-offset out-buffer
                                    (+ n 1)
                                    ;; add the file position to the out-buffer
                                    (repl::ptr-write-ulong rva cur-out))) ; todo fix offset to rva
      ;; return count
      (write-null-hint-name-entry stream)))

;;; PE32

(defun write-idata-import-lookups-pe32-entry (stream name-hint-rva)
  (repl::with-allocation (entry repl::*SIZEOF_ULONG*)
    (repl::ptr-write-ulong name-hint-rva entry)
    (write-sequence (repl::ptr-read-array entry
                                          repl::*SIZEOF_ULONG*)
                    stream)))

(defun write-idata-import-lookups-pe32-entries (stream package idata-offset lib name-hint-offsets lookup-rvas &optional (n 0))
  (let* ((import (repl::package-get-import package n))
         (offset (* n repl::*SIZEOF_ULONG*))
         (nh-rva (+ name-hint-offsets offset)))
    (if import
        (progn
          (if (eq lib (repl::import-entry-lib import))
              (write-idata-import-lookups-pe32-entry stream (repl::ptr-read-ulong nh-rva)))
          (write-idata-import-lookups-pe32-entries stream package idata-offset lib name-hint-offsets lookup-rvas (+ n 1)))
        (write-idata-import-lookups-pe32-entry stream 0)
        )))

(defun write-idata-import-lookups-pe32 (stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas &optional (lib 0))
  ;; each DLL has a list of symbols to import
  (if (< lib (repl::package-libs-count package))
      (progn
        (repl::ptr-write-ulong (+ idata-segment (- (file-position stream) import-segment-offset))
                               (+ lookup-rvas (* lib repl::*SIZEOF_ULONG*)))
        ;; write the lookup table entries
        (write-idata-import-lookups-pe32-entries stream package import-segment-offset lib name-hint-offsets lookup-rvas)
        ;; next
        (write-idata-import-lookups-pe32 stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas
                                         (+ lib 1)))
      ;; return count
      lib))

(defun rewrite-idata-import-lookups-pe32 (stream package idata-segment import-segment-offset lookup-offset name-hint-offsets lookup-rvas)
  (file-excursion stream lookup-offset (lambda ()
                                         (write-idata-import-lookups-pe32 stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas))))

(defun write-empty-pe32-import-lookup-table-entries (stream num-imports)
  (write-n-times 0
                 (* (+ 1 num-imports) repl::*SIZEOF_ULONG*)
                 stream))

(defun write-empty-pe32-import-lookup-tables (stream package &optional (lib 0))
  (if (< lib (repl::package-libs-count package))
      (progn
        (write-empty-pe32-import-lookup-table-entries stream (repl::package-count-imports-from-lib package lib))
        (write-empty-pe32-import-lookup-tables stream package (+ lib 1)))))

;;; PE32+

(defun write-idata-import-lookups-pe32+-entry (stream name-hint-rva)
  (repl::with-allocation (entry repl::*SIZEOF_ULONG64*)
    (repl::ptr-write-ulong64 name-hint-rva entry)
    (write-sequence (repl::ptr-read-array entry
                                          repl::*SIZEOF_ULONG64*)
                    stream)))

(defun write-idata-import-lookups-pe32+-entries (stream package idata-offset lib name-hint-offsets lookup-rvas &optional (n 0))
  (let* ((import (repl::package-get-import package n))
         (offset (* n repl::*SIZEOF_ULONG*))
         (nh-rva (+ name-hint-offsets offset)))
    (if import
        (progn
          (if (eq lib (repl::import-entry-lib import))
              (write-idata-import-lookups-pe32+-entry stream (repl::ptr-read-ulong nh-rva)))
          (write-idata-import-lookups-pe32+-entries stream package idata-offset lib name-hint-offsets lookup-rvas (+ n 1)))
        (write-idata-import-lookups-pe32+-entry stream 0)
        )))

(defun write-idata-import-lookups-pe32+ (stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas &optional (lib 0))
  ;; each DLL has a list of symbols to import
  (if (< lib (repl::package-libs-count package))
      (progn
        (repl::ptr-write-ulong (+ idata-segment (- (file-position stream) import-segment-offset))
                               (+ lookup-rvas (* lib repl::*SIZEOF_ULONG*)))
        ;; write the lookup table entries
        (write-idata-import-lookups-pe32+-entries stream package import-segment-offset lib name-hint-offsets lookup-rvas)
        ;; next
        (write-idata-import-lookups-pe32+ stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas
                                          (+ lib 1)))
      ;; return count
      lib))

(defun rewrite-idata-import-lookups-pe32+ (stream package idata-segment import-segment-offset lookup-offset name-hint-offsets lookup-rvas)
  (file-excursion stream lookup-offset (lambda ()
                                         (write-idata-import-lookups-pe32+ stream package idata-segment import-segment-offset name-hint-offsets lookup-rvas))))

(defun write-empty-pe32+-import-lookup-table-entries (stream num-imports)
  (write-n-times 0
                 (* (+ 1 num-imports) repl::*SIZEOF_ULONG64*)
                 stream))

(defun write-empty-pe32+-import-lookup-tables (stream package &optional (lib 0))
  (if (< lib (repl::package-libs-count package))
      (progn
        (write-empty-pe32+-import-lookup-table-entries stream (repl::package-count-imports-from-lib package lib))
        (write-empty-pe32+-import-lookup-tables stream package (+ lib 1)))))

(defun write-empty-idata-import-table (stream package)
  (write-n-times 0
                 (* (+ 1 (repl::package-libs-count package))
                    (pe-import-table-entry-size))
                 stream))


(defun write-idata-import-table-rvas-inner (stream package lookup-rvas name-rvas address-rvas &optional (lib 0))
  ;; the import table is by DLL
  ;; write pe-import-table-entry for each lib using the corresponding lookup-rva
  (if (< lib (repl::package-libs-count package))
      (repl::with-allocation (entry (pe-import-table-entry-size))
        (set-pe-import-table-entry-lookup-rva entry (repl::ptr-read-ulong lookup-rvas))
        (set-pe-import-table-entry-timedate-stamp entry 0)
        (set-pe-import-table-entry-forwarder-chain entry 0)
        (set-pe-import-table-entry-name-rva entry (repl::ptr-read-ulong name-rvas))
        (set-pe-import-table-entry-address-rva entry (repl::ptr-read-ulong address-rvas))
        (write-sequence (repl::ptr-read-array entry (pe-import-table-entry-size))
                        stream)
        (write-idata-import-table-rvas-inner
         stream
         package
         (+ lookup-rvas repl::*SIZEOF_LONG*)
         (+ name-rvas repl::*SIZEOF_LONG*)
         (+ address-rvas repl::*SIZEOF_LONG*)
         (+ lib 1)))
      lib)
  )

(defun write-idata-import-table-rvas (stream package import-table-offset lookup-rvas name-rvas address-rvas)
  (file-excursion stream import-table-offset (lambda ()
                                               (write-idata-import-table-rvas-inner stream package lookup-rvas name-rvas address-rvas))))

(defun write-idata-library-names (stream package idata-segment idata-section-offset name-rvas &optional (lib 0))
  (if (< lib (repl::package-libs-count package))
      (let* ((name (repl::ptr-read-ptr (repl::package-get-lib package lib)))
             (name-rva (+ idata-segment
                          (- (file-position stream)
                             idata-section-offset))))
        (write-sequence (repl::ptr-read-array name
                                              (+ 1 (length (repl::package-get-lib-name package lib))))
                        stream)
        (write-idata-library-names stream
                                   package
                                   idata-segment
                                   idata-section-offset
                                   (repl::ptr-write-ulong name-rva name-rvas)
                                   (+ lib 1)))
      lib))

(defun write-idata-segment-data (exe stream package pe32+)
  ;; write lookup tables
  (let ((file-start-pos (file-position stream))
        (idata-segment (pe-writer-state-next-mem-page exe))
        (section-offset (pe-writer-state-get-segment-header exe)))
    (repl::with-allocation (import-rvas 1024)
      (write-empty-idata-import-table stream package)
      (write-aligned-padding stream 2)
      (let ((import-lookup-offset (file-position stream)))
        (if pe32+
            (write-empty-pe32+-import-lookup-tables stream package)
            (write-empty-pe32-import-lookup-tables stream package))
        (let* ((table-size (- (file-position stream)
                              file-start-pos))
               (address-offset (file-position stream))
               (address-rva (+ idata-segment (- address-offset section-offset))))
          ;; write empty lookup table, then hint names, then the filled in lookup tables
          (if pe32+
              (write-empty-pe32+-import-lookup-tables stream package)
              (write-empty-pe32-import-lookup-tables stream package))
          (let ((address-size (file-position stream)))
            ;; todo hint names need to collect offsets
            (write-idata-hint-names stream package idata-segment file-start-pos import-rvas)
            (repl::with-allocation (name-rvas 1024)
              (write-idata-library-names stream package idata-segment file-start-pos name-rvas)
              (let* ((section-end (file-position stream))
                     (section-size (- section-end file-start-pos)))
                (repl::with-allocation (lookup-rvas 1024)
                  (repl::with-allocation (address-rvas 1024)
                    ;; fill in the import lookup and address tables
                    (if pe32+
                        (progn
                          (rewrite-idata-import-lookups-pe32+ stream package idata-segment file-start-pos import-lookup-offset import-rvas lookup-rvas)
                          (rewrite-idata-import-lookups-pe32+ stream package idata-segment file-start-pos address-offset import-rvas address-rvas))
                        (progn
                          (rewrite-idata-import-lookups-pe32 stream package idata-segment file-start-pos import-lookup-offset import-rvas lookup-rvas)
                          (rewrite-idata-import-lookups-pe32 stream package idata-segment file-start-pos address-offset import-rvas address-rvas)))
                    ;; write import table
                    (write-idata-import-table-rvas stream package file-start-pos lookup-rvas name-rvas address-rvas)
                    ;; padding
                    (write-aligned-padding stream (pe-writer-state-file-alignment exe))
                    ;; fix segment header rva
                    ;; update data directory entries
                    (if pe32+
                        (progn
                          (patch-pe32+-data-directory stream
                                                      (pe-writer-state-pe32-header-offset exe)
                                                      PE-DATA-DIRECTORY-IMPORT-TABLE
                                                      idata-segment
                                                      table-size)
                          (patch-pe32+-data-directory stream
                                                      (pe-writer-state-pe32-header-offset exe)
                                                      PE-DATA-DIRECTORY-IAT
                                                      address-rva
                                                      address-size))
                        (progn
                          (patch-pe32-data-directory stream
                                                     (pe-writer-state-pe32-header-offset exe)
                                                     PE-DATA-DIRECTORY-IMPORT-TABLE
                                                     idata-segment
                                                     table-size)
                          (patch-pe32-data-directory stream
                                                     (pe-writer-state-pe32-header-offset exe)
                                                     PE-DATA-DIRECTORY-IAT
                                                     address-rva
                                                     address-size)))
                    (values file-start-pos section-size (- (file-position stream) file-start-pos))))))))))))
