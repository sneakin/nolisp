(in-package :repl-windows)

(defun write-exe-header (stream &optional (pe-offset (exe-header-size)) (size pe-offset))
  (repl::with-allocation (header (exe-header-size))
    (repl::ptr-set header (exe-header-size) 0)
    (set-exe-header-blocks-in-file header (floor (/ size 512)))
    (set-exe-header-last-block-size header (mod size 512))
    (set-exe-header-header-paragraphs header (/ (exe-header-size) 16))
    (set-exe-header-signature header EXE-SIGNATURE-SHORT)
    (set-exe-header-pe-header-offset header pe-offset)
    (set-exe-header-ss header #x0000)
    (set-exe-header-sp header #x00B8)
    (set-exe-header-cs header #x0000)
    (set-exe-header-ip header #x0000)
    (write-sequence (repl:ptr-read-array header (exe-header-size))
                    stream)))

(defun write-dos-header (stream)
  (repl::with-allocation (stub 512)
    (let* ((stub-end (msdos-stub stub))
           (stub-size (- stub-end stub)))
      (write-exe-header stream
                        (repl::align-bytes (+ (exe-header-size) stub-size) 128)
                        (+ (exe-header-size) stub-size))
      (write-sequence (repl::ptr-read-array stub stub-size)
                      stream)
      (write-padding stream 128))))



(defun write-pe-header (stream machine num-sections
                        &optional
                          (pe32+ nil)
                          (characteristics (logior PE-IMAGE-FILE-EXECUTABLE-IMAGE
                                                   PE-IMAGE-FILE-LARGE-ADDRESS-AWARE
                                                   PE-IMAGE-FILE-RELOCS-STRIPPED
                                                   PE-IMAGE-FILE-LINE-NUMS-STRIPPED
                                                   PE-IMAGE-FILE-LOCAL-SYMS-STRIPPED
                                                   PE-IMAGE-FILE-DEBUG-STRIPPED)))
  (repl::with-allocation (header (pe-header-size))
    (repl::ptr-set header (pe-header-size) 0)
    (set-pe-header-signature header PE-SIGNATURE-LONG)
    (set-pe-header-machine header machine)
    (set-pe-header-number-sections header num-sections)
    (set-pe-header-timedate-stamp header (get-unix-time))
    (set-pe-header-size-optional-header header (+ (pe-optional-header-size)
                                                  (if pe32+
                                                      (pe-optional-header-pe32+-size)
                                                      (pe-optional-header-pe32-size))
                                                  ))
    (set-pe-header-characteristics header characteristics)
    (write-sequence (repl:ptr-read-array header (pe-header-size))
                    stream)))

(defun write-pe-optional-header (stream cs code-size constants-size bss-size entry-point &optional (magic PE-OPTIONAL-HEADER-PE32))
  (repl::with-allocation (header (pe-optional-header-size))
    (repl::ptr-set header (pe-optional-header-size) 0)
    (set-pe-optional-header-magic header magic)
    (set-pe-optional-header-major-linker-version header 1)
    (set-pe-optional-header-minor-linker-version header 0)
    (set-pe-optional-header-size-code header code-size)
    (set-pe-optional-header-size-initialized-data header constants-size)
    (set-pe-optional-header-size-uninitialized-data header bss-size)
    (set-pe-optional-header-address-entry-point header (+ cs entry-point))
    (set-pe-optional-header-base-of-code header cs)
    (write-sequence (repl:ptr-read-array header (pe-optional-header-size))
                    stream))
  )

(defun file-excursion (stream offset fn)
  (let ((pos (file-position stream)))
    (unwind-protect
         (progn
           (file-position stream offset)
           (funcall fn))
      (file-position stream pos))))

(defun write-empty-pe-optional-header (stream)
  (write-pe-optional-header stream 0 0 0 0 0))

(defun rewrite-pe-optional-header (stream offset cs code-size constants-size bss-size entry-point &optional (magic PE-OPTIONAL-HEADER-PE32))
  (file-excursion stream offset (lambda ()
                                  (write-pe-optional-header stream cs code-size constants-size bss-size entry-point magic))))

(defconstant DEFAULT-IMAGE-BASE (* 4 1024 1024))
(defconstant DEFAULT-CS-BASE (* 1024 1024 4))
(defconstant DEFAULT-STRING-SEGMENT (* 10 1024 1024))
(defconstant DEFAULT-SYMBOL-SEGMENT (* 9 1024 1024))
(defconstant DEFAULT-BSS-SEGMENT (* 20 1024 1024))
(defconstant DEFAULT-IDATA-SEGMENT (* 30 1024 1024))
(defconstant DEFAULT-IDATA-ADDRESS-SEGMENT (* 35 1024 1024))

(defconstant PAGE-SIZE 4096)
(defconstant DEFAULT-SECTION-ALIGNMENT 4096)
(defconstant DEFAULT-FILE-ALIGNMENT 512)

(defconstant UNIX-EPOCH (encode-universal-time 0 0 0 1 1 1970))

(defun write-ulong (value stream)
  (repl::with-allocation (b repl::*SIZEOF_LONG*)
    (repl::ptr-write-ulong value b)
    (write-sequence (repl::ptr-read-array b repl::*SIZEOF_LONG*)
                    stream)))

(defun get-unix-time ()
  (- (get-universal-time)
     UNIX-EPOCH))

(defun write-pe-optional-pe32-header (stream subsystem)
  (repl::with-allocation (header (pe-optional-header-pe32-size))
    (repl::ptr-set header (pe-optional-header-pe32-size) 0)
    (set-pe-optional-header-pe32-image-base header DEFAULT-IMAGE-BASE)
    (set-pe-optional-header-pe32-section-alignment header DEFAULT-SECTION-ALIGNMENT)
    (set-pe-optional-header-pe32-file-alignment header DEFAULT-FILE-ALIGNMENT)
    (set-pe-optional-header-pe32-major-os-version header 4)
    (set-pe-optional-header-pe32-minor-os-version header 0)
    (set-pe-optional-header-pe32-major-subsystem-version header 4)
    (set-pe-optional-header-pe32-minor-subsystem-version header 0)
    (set-pe-optional-header-pe32-size-image header 0) ; fix in post
    (set-pe-optional-header-pe32-size-headers header 0)
    (set-pe-optional-header-pe32-subsystem header subsystem)
    (set-pe-optional-header-pe32-dll-characteristics header 0) ;; todo
    (set-pe-optional-header-pe32-size-stack-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32-size-stack-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32-size-heap-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32-size-heap-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32-number-directories header 16)
    (write-sequence (ptr-read-array header (pe-optional-header-pe32-size))
                    stream)))

(defun pe32-data-directory-offset (pe-header data-directory)
  (pe-optional-header-pe32-directories-ref pe-header data-directory))

(defun patch-pe32-data-directory-inner (stream pe-header data-directory rva size)
  (repl::with-allocation (bytes (pe-data-directory-entry-size))
    (set-pe-data-directory-entry-rva bytes rva)
    (set-pe-data-directory-entry-entry-size bytes size)
    (file-position stream (pe32-data-directory-offset pe-header data-directory))
    (write-sequence (repl::ptr-read-array bytes (pe-data-directory-entry-size))
                    stream)))

(defun patch-pe32-data-directory (stream pe-header data-directory rva size)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32-data-directory-inner stream pe-header data-directory rva size))))

(defun patch-pe32-header-long-inner (stream pe-header slot value)
  (file-position stream (+ pe-header (pe-optional-header-pe32-slot-offset slot)))
  (write-ulong value stream))

(defun patch-pe32-header-long (stream pe-header slot value)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32-header-long-inner stream pe-header slot value))))

(defun write-pe-optional-pe32+-header (stream subsystem)
  (repl::with-allocation (header (pe-optional-header-pe32+-size))
    (repl::ptr-set header (pe-optional-header-pe32+-size) 0)
    (set-pe-optional-header-pe32+-image-base header DEFAULT-IMAGE-BASE)
    (set-pe-optional-header-pe32+-section-alignment header DEFAULT-SECTION-ALIGNMENT)
    (set-pe-optional-header-pe32+-file-alignment header DEFAULT-FILE-ALIGNMENT)
    (set-pe-optional-header-pe32+-major-os-version header 5)
    (set-pe-optional-header-pe32+-minor-os-version header 0)
    (set-pe-optional-header-pe32+-major-subsystem-version header 5)
    (set-pe-optional-header-pe32+-minor-subsystem-version header 0)
    ;;(set-pe-optional-header-pe32+-size-image header 0) ; fix in post
    ;;(set-pe-optional-header-pe32+-size-headers header 0)
    (set-pe-optional-header-pe32+-subsystem header subsystem)
    (set-pe-optional-header-pe32+-dll-characteristics header 0) ;; todo
    (set-pe-optional-header-pe32+-size-stack-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32+-size-stack-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32+-size-heap-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32+-size-heap-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32+-number-directories header 16)
    (write-sequence (ptr-read-array header (pe-optional-header-pe32+-size))
                    stream)))

(defun pe32+-data-directory-offset (pe-header data-directory)
  (pe-optional-header-pe32+-directories-ref pe-header data-directory))

(defun patch-pe32+-data-directory-inner (stream pe-header data-directory rva size)
  (repl::with-allocation (bytes (pe-data-directory-entry-size))
    (set-pe-data-directory-entry-rva bytes rva)
    (set-pe-data-directory-entry-entry-size bytes size)
    (file-position stream (pe32+-data-directory-offset pe-header data-directory))
    (write-sequence (repl::ptr-read-array bytes (pe-data-directory-entry-size))
                    stream)))

(defun patch-pe32+-data-directory (stream pe-header data-directory rva size)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32+-data-directory-inner stream pe-header data-directory rva size))))

(defun patch-pe32+-header-long-inner (stream pe-header slot value)
  (file-position stream (+ pe-header (pe-optional-header-pe32+-slot-offset slot)))
  (write-ulong value stream))

(defun patch-pe32+-header-long (stream pe-header slot value)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32+-header-long-inner stream pe-header slot value))))

(defun write-segment-header (stream name rva size raw-size characteristics &optional (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT) (position 0))
  (repl::with-allocation (header (pe-section-header-size))
    (set-pe-section-header-name header name)
    (set-pe-section-header-virtual-size header raw-size)
    (set-pe-section-header-virtual-address header rva)
    (set-pe-section-header-size-of-raw-data header (repl::align-bytes raw-size file-alignment))
    (set-pe-section-header-pointer-raw-data header position)
    (set-pe-section-header-characteristics header characteristics)
    (write-sequence (repl::ptr-read-array header (pe-section-header-size))
                    stream)
    ))

(defun write-code-segment (stream size &optional (cs DEFAULT-CS-BASE) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (write-segment-header stream
                        ".text"
                        cs
                        size
                        size
                        (logior PE-SECTION-CNT-CODE
                                PE-SECTION-MEM-EXECUTE
                                PE-SECTION-MEM-READ)
                        alignment
                        file-alignment))

(defun write-package-code-segment (stream package &optional (cs DEFAULT-CS-BASE) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (let ((cs-size (repl::package-code-segment-position package)))
    (write-code-segment stream cs-size cs alignment file-alignment)))


(defun patch-section-string-inner (stream section-offset slot value &optional (len (min 8 (length value))))
  (repl::with-allocation (bytes 1024)
    (repl::ptr-write-string value bytes len)
    (file-position stream (+ section-offset (pe-section-header-slot-offset slot)))
    (write-sequence (repl::ptr-read-array bytes len) stream)))

(defun patch-section-string (stream section-offset slot value &optional (len (min 8 (length value))))
  (file-excursion stream section-offset (lambda ()
                                          (patch-section-string-inner stream section-offset slot value len))))

(defun patch-section-ulong-inner (stream section-offset slot value)
  (repl::with-allocation (bytes repl::*SIZEOF_ULONG*)
    (repl::ptr-write-ulong value bytes)
    (file-position stream (+ section-offset (pe-section-header-slot-offset slot)))
    (write-sequence (repl::ptr-read-array bytes repl::*SIZEOF_ULONG*) stream)))

(defun patch-section-ulong (stream section-offset slot value)
  (file-excursion stream section-offset (lambda ()
                                          (patch-section-ulong-inner stream section-offset slot value))))

(defun write-section-raw-data-pointer (stream section-offset value)
  (patch-section-ulong stream section-offset 'pointer-raw-data value))

(defun write-section-raw-data-size (stream section-offset value)
  (patch-section-ulong stream section-offset 'size-of-raw-data value))

(defun write-section-virtual-size (stream section-offset value)
  (patch-section-ulong stream section-offset 'virtual-size value))

(defun write-section-virtual-address (stream section-offset value)
  (patch-section-ulong stream section-offset 'virtual-address value))

(defun write-section-name (stream section-offset value)
  (patch-section-string stream section-offset 'name value 8))

(defun write-section-characteristics (stream section-offset value)
  (patch-section-ulong stream section-offset 'characteristics value))


(defun write-segment-data (stream section-offset data &optional (num-bytes (length data)) (alignment DEFAULT-FILE-ALIGNMENT))
  (let ((pos (file-position stream)))
    (write-section-raw-data-pointer stream section-offset pos)
    (let* ((size num-bytes)
           (aligned-size (repl::align-bytes size alignment)))
      (write-sequence data stream)
      (write-padding stream alignment)
      (write-section-raw-data-size stream section-offset aligned-size)
      (write-section-virtual-address stream section-offset (repl::align-bytes pos DEFAULT-SECTION-ALIGNMENT))
      (write-section-virtual-size stream section-offset size)
      aligned-size)))


(defun write-code-segment-data (stream package section-offset)
  (write-segment-data stream
                      section-offset
                      (repl::ptr-read-array (repl::package-code-segment-buffer package)
                                            (repl::package-code-segment-position package))))

(defun write-fake-code-segment-data (stream package section-offset)
  (repl::with-allocation (dummy-code 1024)
    (repl::ptr-set dummy-code 1024 #x90) ; nops
    (let ((end-offset (repl-amd64::emit-ops-64 dummy-code
                                               ;;(:int3)
                                               (push #x2C)
                                               (pop :eax)
                                               (ret)))
          )
      (write-segment-data stream
                          section-offset
                          (repl::ptr-read-array dummy-code (- end-offset dummy-code))))))


(defun write-string-segment (stream package &optional (ss DEFAULT-STRING-SEGMENT) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (let ((ss-size (repl::package-string-segment-position package)))
    (write-segment-header stream
                          ".strings"
                          ss
                          ss-size
                          ss-size
                          (logior PE-SECTION-CNT-INITIALIZED-DATA
                                  PE-SECTION-MEM-READ)
                          alignment
                          file-alignment)
    ))

(defun write-string-segment-data (stream package section-offset)
  (write-segment-data stream
                      section-offset
                      (repl::ptr-read-array (repl::package-string-segment-data package)
                                            (repl::package-string-segment-position package))))

(defun write-symbol-segment (stream package &optional (ss DEFAULT-SYMBOL-SEGMENT) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (let ((ss-size (repl::package-symbols-size package)))
    (write-segment-header stream
                          ".symbols"
                          ss
                          ss-size
                          ss-size
                          (logior PE-SECTION-CNT-INITIALIZED-DATA
                                  PE-SECTION-MEM-READ)
                          alignment
                          file-alignment)
    ))

(defun write-symbol-segment-data (stream package section-offset)
  (write-segment-data stream
                      section-offset
                      (repl::ptr-read-array (repl::package-symbols-buffer package)
                                            (repl::package-symbols-size package))))

(defun write-bss-segment (stream package size &optional (rva DEFAULT-BSS-SEGMENT) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (write-segment-header stream
                        ".bss"
                        rva
                        size
                        0
                        (logior PE-SECTION-CNT-UNINITIALIZED-DATA
                                PE-SECTION-MEM-READ
                                PE-SECTION-MEM-WRITE)
                        alignment
                        file-alignment))

(defun write-idata-segment (stream package &optional (rva DEFAULT-IDATA-SEGMENT) (alignment 4096) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (write-segment-header stream
                        ".idata"
                        rva
                        (repl::package-imports-size package)
                        (repl::package-imports-size package)
                        (logior PE-SECTION-CNT-INITIALIZED-DATA
                                PE-SECTION-MEM-WRITE
                                PE-SECTION-MEM-READ)
                        alignment
                        file-alignment)
)

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

(defun write-idata-hint-names (stream package import-segment-offset out-buffer &optional (n 0) (cur-out out-buffer))
  ;; fixme can be a flat list, need to collect file offsets for import-lookups
  ;; each DLL has a list of symbols to import
  (if (< n (repl::package-imports-count package))
      (let* ((offset (file-position stream))
             (rva (+ DEFAULT-IDATA-SEGMENT
                     (- offset import-segment-offset))))
        ;; write the pe-hint-name
        (write-idata-hint-name-entry stream (repl::package-get-import package n))
        ;; next
        (write-idata-hint-names stream package import-segment-offset out-buffer
                                    (+ n 1)
                                    ;; add the file position to the out-buffer
                                    (repl::ptr-write-ulong rva cur-out))) ; todo fix offset to rva
      ;; return count
      (write-null-hint-name-entry stream)))

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
              (progn
                (format *standard-output* "Import lookup: ~A ~A ~x ~x ~x~%" lib n offset (repl::ptr-read-ulong nh-rva) (+ DEFAULT-IDATA-SEGMENT (- (file-position stream) idata-offset)))
                (write-idata-import-lookups-pe32+-entry stream (repl::ptr-read-ulong nh-rva))))
          (write-idata-import-lookups-pe32+-entries stream package idata-offset lib name-hint-offsets lookup-rvas (+ n 1)))
        (write-idata-import-lookups-pe32+-entry stream 0)
        )))

(defun write-idata-import-lookups-pe32+ (stream package import-segment-offset name-hint-offsets lookup-rvas &optional (lib 0))
  ;; each DLL has a list of symbols to import
  (if (< lib (repl::package-libs-count package))
      (progn
        (format *standard-output* "Importing from ~A ~x~%" (repl::package-get-lib-name package lib) (+ DEFAULT-IDATA-SEGMENT (- (file-position stream) import-segment-offset)))
        (repl::ptr-write-ulong (+ DEFAULT-IDATA-SEGMENT (- (file-position stream) import-segment-offset))
                               (+ lookup-rvas (* lib repl::*SIZEOF_ULONG*)))
        ;; write the lookup table entries
        (write-idata-import-lookups-pe32+-entries stream package import-segment-offset lib name-hint-offsets lookup-rvas)
        ;; next
        (write-idata-import-lookups-pe32+ stream package import-segment-offset name-hint-offsets lookup-rvas
                                          (+ lib 1)))
      ;; return count
      lib))

(defun rewrite-idata-import-lookups-pe32+ (stream package import-segment-offset lookup-offset name-hint-offsets lookup-rvas)
  (file-excursion stream lookup-offset (lambda ()
                                         (write-idata-import-lookups-pe32+ stream package import-segment-offset name-hint-offsets lookup-rvas))))

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
  ;; write pe-import-table-entry for each lib using the corresponding lookup-rva
  )

(defun write-idata-import-table-rvas (stream package import-table-offset lookup-rvas name-rvas address-rvas)
  (file-excursion stream import-table-offset (lambda ()
                                               (write-idata-import-table-rvas-inner stream package lookup-rvas name-rvas address-rvas))))

(defun write-idata-library-names (stream package idata-section-offset name-rvas &optional (lib 0))
  (if (< lib (repl::package-libs-count package))
      (let* ((name (repl::ptr-read-ptr (repl::package-get-lib package lib)))
             (name-rva (+ DEFAULT-IDATA-SEGMENT
                          (- (file-position stream)
                             idata-section-offset))))
        (format *standard-output* "Lib ~A: ~A ~A~%" lib name name-rva)
        (write-sequence (repl::ptr-read-array name
                                              (+ 1 (length (repl::package-get-lib-name package lib))))
                        stream)
        (write-idata-library-names stream
                                   package
                                   idata-section-offset
                                   (repl::ptr-write-ulong name-rva name-rvas)
                                   (+ lib 1)))
      lib))

(defun write-idata-segment-data (stream package section-offset)
  ;; write lookup tables
  (let ((file-start-pos (file-position stream)))
    (repl::with-allocation (import-rvas 1024)
      (write-empty-idata-import-table stream package)
      (write-padding stream 2)
      (let ((import-lookup-offset (file-position stream)))
        (write-empty-pe32+-import-lookup-tables stream package)
        (let* ((table-size (- (file-position stream)
                              file-start-pos))
               (address-offset (file-position stream))
               (address-rva (+ DEFAULT-IDATA-SEGMENT (- address-offset section-offset))))
          (write-empty-pe32+-import-lookup-tables stream package)
          ;; todo verify write empty lookup table, then hint names, then the filled in lookup tables
          (let ((address-size (file-position stream)))
            ;; todo hint names need to collect offsets
            (write-idata-hint-names stream package file-start-pos import-rvas)
            (repl::with-allocation (name-rvas 1024)
              (write-idata-library-names stream package file-start-pos name-rvas)
              (let* ((section-end (file-position stream))
                     (section-size (- section-end file-start-pos)))
                (repl::with-allocation (lookup-rvas 1024)
                  (repl::with-allocation (address-rvas 1024)
                    ;; fill in the import lookup and address tables
                    (rewrite-idata-import-lookups-pe32+ stream package file-start-pos import-lookup-offset import-rvas lookup-rvas)
                    (rewrite-idata-import-lookups-pe32+ stream package file-start-pos address-offset import-rvas address-rvas)
                    ;; write import table
                    (write-idata-import-table-rvas stream package file-start-pos lookup-rvas name-rvas address-rvas)
                    ;; padding
                    (write-padding stream DEFAULT-FILE-ALIGNMENT)
                    ;; fix segment header rva
                    (write-section-virtual-size stream section-offset section-size)
                    (write-section-raw-data-size stream section-offset (repl::align-bytes section-size DEFAULT-FILE-ALIGNMENT))
                    (write-section-raw-data-pointer stream section-offset file-start-pos)
                    (values file-start-pos table-size address-rva address-size))
                  )
                )
              )))
        ))))

(defun write-edata-segment (stream package)
  (repl::with-allocation (header (pe-section-header-size))
    (set-pe-section-header-name header ".edata")
    (write-sequence (repl::ptr-read-array header (pe-section-header-size))
                    stream)))

(defun write-n-times (value times stream)
  (if (> times 0)
      (progn
        (write-byte value stream)
        (write-n-times value (- times 1) stream))))

(defun write-aligned-padding (stream boundary)
  (let ((pos (file-position stream)))
    (write-n-times 0 (- (repl::align-bytes pos boundary)
                        pos)
                   stream)))

(defun write-empty-segment-header (stream name)
  (write-segment-header stream
                        name
                        0
                        0
                        0
                        0
                        0))

(defun write-empty-segment-headers (pe stream num)
  (if (> num 0)
      (progn
        (write-empty-segment-header stream "")
        (write-empty-segment-headers pe stream (- num 1)))
      (write-padding stream (pe-writer-state-file-alignment pe))))

(defun write-misc-segment-data-inner (stream &optional (str "Made using NoLisp."))
  (repl::with-allocation (msg 4096)
    (repl::ptr-write-string str msg)
    (write-sequence (repl::ptr-read-array msg (length str))
                    stream)))


(defun write-misc-segment-header-inner (stream header-offset size)
  (file-position stream header-offset)
  (write-segment-header stream
                        ".misc"
                        0
                        size
                        size
                        (logior PE-SECTION-CNT-INITIALIZED-DATA
                                PE-SECTION-MEM-WRITE
                                PE-SECTION-MEM-READ)
                        4096
                        DEFAULT-FILE-ALIGNMENT))

(defun write-misc-segment-header (stream header-offset size)
  (file-excursion stream header-offset (lambda ()
                                         (write-misc-segment-header-inner stream header-offset size))))

(defun write-misc-segment-data (stream header-offset)
  (let ((data-start (file-position stream)))
    (write-misc-segment-data-inner stream)
    (let ((data-size (- (file-position stream)
                        data-start)))
      (write-padding stream DEFAULT-FILE-ALIGNMENT)
      (write-misc-segment-header stream header-offset data-size)
      (write-section-raw-data-pointer stream header-offset data-start))))


(defun write-pe-file (path package)
  ;; write:
  (with-open-file (f path
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (let ((code-size (repl::package-code-segment-position package))
          (bss-size (repl::package-symbols-size package))
          (string-size (repl::package-string-segment-position package))
          (entry-point 0))
      ;; dos header
      (write-dos-header f)
      ;; pe header
      (let ((pe-header-offset (file-position f)))
        (write-pe-header f PE-IMAGE-MACHINE-AMD64 6 t)
        ;;(write-pe-header f PE-IMAGE-MACHINE-BACAW 5)
        ;; optional header
        (let ((pe-opt-header-offset (file-position f)))
          (write-empty-pe-optional-header f)
          (let ((pe32-header-offset (file-position f)))
            (write-pe-optional-pe32+-header f PE-IMAGE-SUBSYS-WINDOWS-CUI)
            ;; segment list followed by the segments' data
            ;; store the header's offset to patch offsets
            (let ((code-segment-offset (file-position f)))
              (write-package-code-segment f package)
              ;; data segments
              (let ((string-segment-offset (file-position f)))
                (write-string-segment f package)
                (let ((symbol-segment-offset (file-position f)))
                  (write-symbol-segment f package)
                  ;; bss has no raw data, no need to store offset
                  (write-bss-segment f package bss-size)
                  ;; imports
                  (let ((idata-offset (file-position f)))
                    (write-idata-segment f package)
                    (let ((misc-offset (file-position f)))
                      (write-empty-segment-header f ".misc")
                      (let ((header-end (file-position f)))
                        (write-misc-segment-data f misc-offset)
                        ;; exports
                        ;;(write-edata-segment f package)
                        ;; write the actual data and fix fields
                        ;; (write-code-segment-data f package code-segment-offset)
                        (write-fake-code-segment-data f package code-segment-offset)
                        (format *standard-output* "code segment: ~A ~A~%" idata-offset (file-position f))
                        (write-string-segment-data f package string-segment-offset)
                        (format *standard-output* "string segment: ~A ~A~%" idata-offset (file-position f))
                        (write-symbol-segment-data f package symbol-segment-offset)
                        (let ((import-offset (file-position f)))
                          (multiple-value-bind (import-table-offset import-size iat-rva iat-size)
                              (write-idata-segment-data f package idata-offset)
                            (let ((file-size (file-position f)))
                              ;; update fields:
                              ;; entry point
                              (patch-pe32+-header-long f pe32-header-offset 'size-image (repl::align-bytes (max (* 6 PAGE-SIZE) file-size) DEFAULT-SECTION-ALIGNMENT))
                              (patch-pe32+-header-long f pe32-header-offset 'size-headers (repl::align-bytes header-end DEFAULT-FILE-ALIGNMENT))
                              (rewrite-pe-optional-header f pe-opt-header-offset (repl::align-bytes code-segment-offset DEFAULT-SECTION-ALIGNMENT) code-size file-size bss-size entry-point PE-OPTIONAL-HEADER-PE32+)
                              ;; data directory entries: edata, idata
                              (patch-pe32+-data-directory f
                                                    pe32-header-offset
                                                    PE-DATA-DIRECTORY-IMPORT-TABLE
                                                    DEFAULT-IDATA-SEGMENT
                                                    import-size)
                              (patch-pe32+-data-directory f
                                                    pe32-header-offset
                                                    PE-DATA-DIRECTORY-IAT
                                                    iat-rva
                                                    iat-size)
                              (file-position f))
                            )
                          ))
                      )))))))
        ))))

(defun write-empty-pe32+-file (path)
  ;; write:
  (with-open-file (f path
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (let ((code-size 0)
          (bss-size 0)
          (string-size 0)
          (entry-point 0))
      ;; dos header
      (write-dos-header f)
      ;; pe header
      (let ((pe-header-offset (file-position f)))
        (write-pe-header f PE-IMAGE-MACHINE-AMD64 1 t)
        ;;(write-pe-header f PE-IMAGE-MACHINE-BACAW 5)
        ;; optional header
        (let ((pe-opt-header-offset (file-position f)))
          (write-empty-pe-optional-header f)
          (let ((pe32-header-offset (file-position f)))
            (write-pe-optional-pe32+-header f PE-IMAGE-SUBSYS-WINDOWS-CUI)
            ;; segment list followed by the segments' data
            ;; store the header's offset to patch offsets
            (let ((code-segment-offset (file-position f)))
              (write-code-segment f 8)
              (write-padding f DEFAULT-FILE-ALIGNMENT)
              (let ((header-end (file-position f)))
                ;; write the actual data and fix fields
                (write-fake-code-segment-data f nil code-segment-offset)
                (let ((file-size (file-position f)))
                  ;; update fields:
                  ;; entry point
                  (patch-pe32+-header-long f pe32-header-offset 'size-image (* 2 PAGE-SIZE))
                  (patch-pe32+-header-long f pe32-header-offset 'size-headers (repl::align-bytes header-end DEFAULT-FILE-ALIGNMENT))
                  (rewrite-pe-optional-header f pe-opt-header-offset (repl::align-bytes header-end DEFAULT-SECTION-ALIGNMENT) code-size 0 bss-size entry-point PE-OPTIONAL-HEADER-PE32+)
                  (file-position f))))))
        ))))

(defun write-empty-pe-file (path)
  ;; write:
  (with-open-file (f path
                     :direction :output
                     :element-type '(unsigned-byte 8))
    (let ((code-size 0)
          (bss-size 0)
          (string-size 0)
          (entry-point 0))
      ;; dos header
      (write-dos-header f)
      ;; pe header
      (let ((pe-header-offset (file-position f)))
        (write-pe-header f PE-IMAGE-MACHINE-I386 1 nil (logior PE-IMAGE-FILE-EXECUTABLE-IMAGE
                                                               PE-IMAGE-FILE-RELOCS-STRIPPED
                                                               PE-IMAGE-FILE-LINE-NUMS-STRIPPED
                                                               PE-IMAGE-FILE-LOCAL-SYMS-STRIPPED
                                                               PE-IMAGE-FILE-DEBUG-STRIPPED))
        ;;(write-pe-header f PE-IMAGE-MACHINE-BACAW 5)
        ;; optional header
        (let ((pe-opt-header-offset (file-position f)))
          (write-empty-pe-optional-header f)
          (let ((pe32-header-offset (file-position f)))
            (write-pe-optional-pe32-header f PE-IMAGE-SUBSYS-WINDOWS-CUI)
            ;; segment list followed by the segments' data
            ;; store the header's offset to patch offsets
            (let ((code-segment-offset (file-position f)))
              (write-code-segment f 8)
              (write-padding f DEFAULT-FILE-ALIGNMENT)
              (let ((header-end (file-position f)))
                ;; write the actual data and fix fields
                (write-fake-code-segment-data f nil code-segment-offset)
                (let ((file-size (file-position f)))
                  ;; update fields:
                  ;; entry point
                  (patch-pe32-header-long f pe32-header-offset 'size-image (repl::align-bytes (max (* 2 PAGE-SIZE) file-size) DEFAULT-SECTION-ALIGNMENT))
                  (patch-pe32-header-long f pe32-header-offset 'size-headers (repl::align-bytes header-end DEFAULT-FILE-ALIGNMENT))
                  (rewrite-pe-optional-header f pe-opt-header-offset (repl::align-bytes header-end DEFAULT-SECTION-ALIGNMENT) code-size 0 bss-size entry-point PE-OPTIONAL-HEADER-PE32)
                  (file-position f))))))
        ))))

(defun with-pe-file (path num-segments fn &optional (image-base DEFAULT-IMAGE-BASE))
  (repl::with-allocation (exe (pe-writer-state-size))
    (pe-writer-state-init exe image-base)
    (with-open-file (stream path
                            :direction :output
                            :element-type '(unsigned-byte 8))
      ;; dos header
      (write-dos-header stream)
      ;; pe header
      (let ((pe-header-offset (file-position stream)))
        (set-pe-writer-state-pe-header-offset exe pe-header-offset)
        (write-pe-header stream PE-IMAGE-MACHINE-I386 num-segments
                         nil
                         (logior PE-IMAGE-FILE-EXECUTABLE-IMAGE
                                 PE-IMAGE-FILE-RELOCS-STRIPPED
                                 PE-IMAGE-FILE-LINE-NUMS-STRIPPED
                                 PE-IMAGE-FILE-LOCAL-SYMS-STRIPPED
                                 PE-IMAGE-FILE-DEBUG-STRIPPED))
        ;;(write-pe-header f PE-IMAGE-MACHINE-BACAW 5)
        ;; optional header
        (let ((pe-opt-header-offset (file-position stream)))
          (set-pe-writer-state-pe-opt-header-offset exe pe-opt-header-offset)
          (write-empty-pe-optional-header stream)
          (let ((pe32-header-offset (file-position stream)))
            (set-pe-writer-state-pe32-header-offset exe pe32-header-offset)
            (write-pe-optional-pe32-header stream PE-IMAGE-SUBSYS-WINDOWS-CUI)
            ;; segment list followed by the segments' data
            ;; store the header's offset to patch offsets
            (let ((segment-headers (file-position stream)))
              (write-empty-segment-headers exe stream num-segments)
              (let ((header-end (file-position stream)))
                (set-pe-writer-state-segment-headers exe segment-headers)
                (set-pe-writer-state-num-segments exe num-segments)
                (set-pe-writer-state-next-file-page exe header-end)
                (set-pe-writer-state-next-mem-page exe (pe-writer-state-mem-alignment exe))
                (funcall fn exe stream)
                (patch-pe32-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-image (pe-writer-state-next-mem-page exe))
                (patch-pe32-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-headers header-end)
                
                (file-position stream)))))))))


(defun pe-writer-state-inc-mem-page (exe size)
  (let* ((now (pe-writer-state-next-mem-page exe))
         (next (+ now
                  (repl::align-bytes size (pe-writer-state-mem-alignment exe)))))
    (set-pe-writer-state-next-mem-page exe next)
    now))

(defun pe-writer-state-inc-code-size (exe size)
  (let* ((now (pe-writer-state-code-size exe))
         (next (+ now size)))
    (set-pe-writer-state-code-size exe next)
    next))

(defun pe-writer-state-inc-data-size (exe size)
  (let* ((now (pe-writer-state-data-size exe))
         (next (+ now size)))
    (set-pe-writer-state-data-size exe next)
    next))

(defun pe-writer-state-next-segment-header (exe)
  (let ((n (pe-writer-state-next-segment exe))
        (offset (pe-writer-state-segment-headers exe)))
    (if (>= n (pe-writer-state-num-segments exe))
        (error 'too-many-segments :number n))
    (set-pe-writer-state-next-segment exe (+ 1 n))
    (+ offset (* n (pe-section-header-size)))))

(defun write-pe-segment (exe stream name data data-len flags)
  (let* ((section-offset (pe-writer-state-next-segment-header exe))
         (mem-page (pe-writer-state-next-mem-page exe))
         (real-size (if data
                        (write-segment-data stream section-offset (repl::ptr-read-array data data-len) data-len)
                        data-len))
         (mem-end (pe-writer-state-inc-mem-page exe real-size)))
    (write-section-virtual-address stream section-offset mem-page)
    (write-section-virtual-size stream section-offset data-len)
    (write-section-name stream section-offset name)
    (write-section-characteristics stream section-offset flags)
    (set-pe-writer-state-next-file-page exe (file-position stream))
    (cond
      ((< 0 (logand flags PE-SECTION-CNT-CODE))
       (pe-writer-state-inc-code-size mem-size))
      ((< 0 (logand flags (logior PE-SECTION-CNT-INITIALIZED-DATA
                                  PE-SECTION-CNT-UNINITIALIZED-DATA)))
       (pe-writer-state-inc-data-size mem-size)))
    (values mem-page
            (- mem-end mem-page))))

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


;; this is the one
(defun write-empty-pe-file (path)
  (with-pe-file path 3 (lambda (exe f)
                         (multiple-value-bind (cs code-size)
                             (write-fake-code-segment exe f)
                           (multiple-value-bind (string-start string-size)
                               (write-fake-string-segment exe f)
                             (write-pe-segment exe f ".bss" nil 12 (logior PE-SECTION-CNT-UNINITIALIZED-DATA
                                                                           PE-SECTION-MEM-READ
                                                                           PE-SECTION-MEM-WRITE))
                             (rewrite-pe-optional-header f
                                                         (pe-writer-state-pe-opt-header-offset exe)
                                                         cs code-size string-size 0 0 PE-OPTIONAL-HEADER-PE32))))))

(defun write-package-to-pe-file (path package)
  (repl::with-allocation (data-buffer MAX-SEGMENT-SIZE)
    (with-pe-file (pe path)
      (write-segment pe
                     ".text"
                     (repl::package-code-segment-data package)
                     (repl::package-code-segment-position package)
                     (logior PE-SECTION-EXEC))
      (write-segment pe
                     ".data"
                     (repl::package-string-segment-data package)
                     (repl::package-string-segment-position package)
                     (logior PE-SECTION-DATA))))
  )
