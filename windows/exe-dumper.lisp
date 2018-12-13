(in-package :repl-windows)

(defun print-dos-header (header stream)
  (format stream "~%EXE Header ~A~%" header)
  (format stream "  signature: ~A ~A~%" (exe-header-signature header) (if (eq (exe-header-signature header) EXE-SIGNATURE-SHORT)
                                                                          "valid"))
  (format stream "  last-block-size: ~A~%" (exe-header-last-block-size header))
  (format stream "  blocks-in-file: ~A~%" (exe-header-blocks-in-file header))
  (format stream "  num-relocs: ~A~%" (exe-header-num-relocs header))
  (format stream "  header-paragraphs: ~A~%" (exe-header-header-paragraphs header))
  (format stream "  min-extra-paragraphs: ~A~%" (exe-header-min-extra-paragraphs header))
  (format stream "  max-extra-paragraphs: ~A~%" (exe-header-max-extra-paragraphs header))
  (format stream "  ss: ~A~%" (exe-header-ss header))
  (format stream "  sp: ~A~%" (exe-header-sp header))
  (format stream "  checksum: ~A~%" (exe-header-checksum header))
  (format stream "  ip: ~A~%" (exe-header-ip header))
  (format stream "  cs: ~A~%" (exe-header-cs header))
  (format stream "  reloc-table-offset: ~A~%" (exe-header-reloc-table-offset header))
  (format stream "  overlay-number: ~A~%" (exe-header-overlay-number header))
  (format stream "  OEM id: ~A~%" (exe-header-oem-id header))
  (format stream "  OEM info: ~A~%" (exe-header-oem-info header))
  (format stream "  PE Header offset: ~A~%" (exe-header-pe-header-offset header))
  (format stream "~%")
  (format stream "  Data start: ~A ~A~%" (exe-data-start header) (exe-data-start-by-blocks header))
  (format stream "  Last data byte: ~A~%" (exe-last-data-byte header)))

(defun print-pe-header (ptr &optional (stream *standard-output*))
  (format stream "~%PE Header ~A~%" ptr)
  (format stream "  Signature: ~A ~A~%" (pe-header-signature ptr) (if (= PE-SIGNATURE-LONG (pe-header-signature ptr))
                                                                    "valid"))
  (format stream "  Machine: ~A ~A~%" (pe-header-machine ptr) (pe-image-machine-string (pe-header-machine ptr)))
  (format stream "  Number sections: ~A~%" (pe-header-number-sections ptr))
  (format stream "  Timedate Stamp: ~A~%" (pe-header-timedate-stamp ptr))
  (format stream "  Symbol table pointer: ~A~%" (pe-header-symbol-table-pointer ptr))
  (format stream "  Number symbols: ~A~%" (pe-header-number-symbols ptr))
  (format stream "  Size optional header: ~A~%" (pe-header-size-optional-header ptr))
  (format stream "  Characteristics: 0x~x~%" (pe-header-characteristics ptr)))

(defun print-import-table-names (name-tbl section-base ptr-size stream &optional (n 0))
  (let* ((file-offset (- name-tbl section-base))
         (import-rva (repl:ptr-read-ulong file-offset))
         (import-offset (- import-rva section-base)))
    (if (> 0 import-offset)
        (format stream "~A: null ~A ~x, ~A ~x~%" n file-offset file-offset import-rva import-rva)
        (let* ((hint (pe-hint-name-hint import-offset))
               (name (pe-hint-name-string import-offset)))
          (format stream "~A: ~A ~x, ~A ~x, ~A ~x, ~A, ~A~%" n file-offset file-offset import-rva import-rva hint hint name (pe-hint-name-by-ordinal? import-offset ptr-size))
          (print-import-table-names (+ name-tbl repl::*SIZEOF_ULONG64*)
                                    section-base
                                    ptr-size
                                    stream
                                    (+ n 1))))))

(defun print-import-table (tbl ptr-size stream &optional (section-base 0) (n 0))
  (format stream "~%Import table entry ~a @ ~A   Section: ~x~%" n tbl section-base)
  (format stream "  Lookup RVA: ~A ~x~%" (pe-import-table-entry-lookup-rva tbl) (pe-import-table-entry-lookup-rva tbl))
  (format stream "  Timedate: ~A~%" (pe-import-table-entry-timedate-stamp tbl))
  (format stream "  Forwarder: ~A~%" (pe-import-table-entry-forwarder-chain tbl))
  (format stream "  Name RVA: ~A ~A~%" (pe-import-table-entry-name-rva tbl) (handler-case (ptr-read-string (- (pe-import-table-entry-name-rva tbl) section-base) 32)
                                                                              (t nil)))
  (format stream "  Address RVA: ~A ~x~%" (pe-import-table-entry-address-rva tbl) (pe-import-table-entry-address-rva tbl))
  (format stream "~%    Imports: ~A ~x~%" (pe-import-table-entry-lookup-rva tbl) (pe-import-table-entry-lookup-rva tbl))
  (print-import-table-names (pe-import-table-entry-lookup-rva tbl) section-base ptr-size stream)
  (format stream "~%    Addresses: ~A ~x~%" (pe-import-table-entry-address-rva tbl) (pe-import-table-entry-address-rva tbl))
  (print-import-table-names (pe-import-table-entry-address-rva tbl) section-base ptr-size stream)
  (let ((next (+ tbl (pe-import-table-entry-size))))
    (if (< 0 (pe-import-table-entry-lookup-rva next))
        (print-import-table next ptr-size stream section-base (+ n 1)))))


(defun print-data-directory (n directory stream)
  (format stream "  ~A ~A: ~A bytes @ ~A 0x~x~%"
          n
          (pe-data-directory-string n)
          (pe-data-directory-entry-entry-size directory)
          (pe-data-directory-entry-rva directory)
          (pe-data-directory-entry-rva directory)))

(defun print-data-directories (num-directories directories stream &optional (n 0))
  (print-data-directory n directories stream)
  (if (< n num-directories)
      (print-data-directories num-directories
                              (+ directories (pe-data-directory-entry-size))
                              stream
                              (+ n 1))))

(defun print-optional-header-pe32 (header stream)
  (format stream "  base-of-data: ~A~%" (pe-optional-header-pe32-base-of-data header))
  (format stream "  image-base: ~A~%" (pe-optional-header-pe32-image-base header))
  (format stream "  section-alignment: ~A~%" (pe-optional-header-pe32-section-alignment header))
  (format stream "  file-alignment: ~A~%" (pe-optional-header-pe32-file-alignment header))
  (format stream "  major-os-version: ~A~%" (pe-optional-header-pe32-major-os-version header))
  (format stream "  minor-os-version: ~A~%" (pe-optional-header-pe32-minor-os-version header))
  (format stream "  major-image-version: ~A~%" (pe-optional-header-pe32-major-image-version header))
  (format stream "  minor-image-version: ~A~%" (pe-optional-header-pe32-minor-image-version header))
  (format stream "  major-subsystem-version: ~A~%" (pe-optional-header-pe32-major-subsystem-version header))
  (format stream "  minor-subsystem-version: ~A~%" (pe-optional-header-pe32-minor-subsystem-version header))
  (format stream "  win32-version: ~A~%" (pe-optional-header-pe32-win32-version header))
  (format stream "  size-image: ~A~%" (pe-optional-header-pe32-size-image header))
  (format stream "  size-headers: ~A~%" (pe-optional-header-pe32-size-headers header))
  (format stream "  checksum: ~A~%" (pe-optional-header-pe32-checksum header))
  (format stream "  subsystem: ~A~%" (pe-optional-header-pe32-subsystem header))
  (format stream "  dll-characteristics: ~A~%" (pe-optional-header-pe32-dll-characteristics header))
  (format stream "  size-stack-reserve: ~A~%" (pe-optional-header-pe32-size-stack-reserve header))
  (format stream "  size-stack-commit: ~A~%" (pe-optional-header-pe32-size-stack-commit header))
  (format stream "  size-heap-reserve: ~A~%" (pe-optional-header-pe32-size-heap-reserve header))
  (format stream "  size-heap-commit: ~A~%" (pe-optional-header-pe32-size-heap-commit header))
  (format stream "  loader-flags: ~A~%" (pe-optional-header-pe32-loader-flags header))
  (format stream "  number-directories: ~A~%" (pe-optional-header-pe32-number-directories header))
                                        ; (directories :type pe-data-directory-entry :size 16)
  (print-data-directories (pe-optional-header-pe32+-number-directories header)
                          (pe-optional-header-pe32+-directories-ref header)
                          stream))

(defun print-optional-header-pe32+ (header stream)
  (format stream "  image-base: ~A~%" (pe-optional-header-pe32+-image-base header))
  (format stream "  section-alignment: ~A~%" (pe-optional-header-pe32+-section-alignment header))
  (format stream "  file-alignment: ~A~%" (pe-optional-header-pe32+-file-alignment header))
  (format stream "  major-os-version: ~A~%" (pe-optional-header-pe32+-major-os-version header))
  (format stream "  minor-os-version: ~A~%" (pe-optional-header-pe32+-minor-os-version header))
  (format stream "  major-image-version: ~A~%" (pe-optional-header-pe32+-major-image-version header))
  (format stream "  minor-image-version: ~A~%" (pe-optional-header-pe32+-minor-image-version header))
  (format stream "  major-subsystem-version: ~A~%" (pe-optional-header-pe32+-major-subsystem-version header))
  (format stream "  minor-subsystem-version: ~A~%" (pe-optional-header-pe32+-minor-subsystem-version header))
  (format stream "  win32-version: ~A~%" (pe-optional-header-pe32+-win32-version header))
  (format stream "  size-image: ~A~%" (pe-optional-header-pe32+-size-image header))
  (format stream "  size-headers: ~A~%" (pe-optional-header-pe32+-size-headers header))
  (format stream "  checksum: ~A~%" (pe-optional-header-pe32+-checksum header))
  (format stream "  subsystem: ~A~%" (pe-optional-header-pe32+-subsystem header))
  (format stream "  dll-characteristics: ~A~%" (pe-optional-header-pe32+-dll-characteristics header))
  (format stream "  size-stack-reserve: ~A~%" (pe-optional-header-pe32+-size-stack-reserve header))
  (format stream "  size-stack-commit: ~A~%" (pe-optional-header-pe32+-size-stack-commit header))
  (format stream "  size-heap-reserve: ~A~%" (pe-optional-header-pe32+-size-heap-reserve header))
  (format stream "  size-heap-commit: ~A~%" (pe-optional-header-pe32+-size-heap-commit header))
  (format stream "  loader-flags: ~A~%" (pe-optional-header-pe32+-loader-flags header))
  (format stream "  number-directories: ~A~%" (pe-optional-header-pe32+-number-directories header))
  (print-data-directories (pe-optional-header-pe32+-number-directories header)
                          (pe-optional-header-pe32+-directories-ref header)
                          stream))

(defun print-optional-header (header &optional (stream *standard-output*))
  (format stream "~%Optional header ~A~%" header)
  (format stream "  Magic: ~A~%" (if (eq PE-OPTIONAL-HEADER-PE32 (pe-optional-header-magic header))
                                     "PE32"
                                     "PE32+"))
  (format stream "  Linker version: ~A.~A~%" (pe-optional-header-major-linker-version header) (pe-optional-header-minor-linker-version header))
  (format stream "  Code size: ~A~%" (pe-optional-header-size-code header))
  (format stream "  Initialized data: ~A~%" (pe-optional-header-size-initialized-data header))
  (format stream "  Uninitialized data: ~A~%" (pe-optional-header-size-uninitialized-data header))
  (format stream "  Entry point: ~A~%" (pe-optional-header-address-entry-point header))
  (format stream "  Base of code: ~A~%" (pe-optional-header-base-of-code header))
  (if (eq PE-OPTIONAL-HEADER-PE32 (pe-optional-header-magic header))
      (print-optional-header-pe32 (+ header (pe-optional-header-size)) stream)
      (print-optional-header-pe32+ (+ header (pe-optional-header-size)) stream)))


(defun print-section (image-base section num-sections ptr-size stream)
  (format stream "  ~A: ~A~%" num-sections (ptr-read-string (pe-section-header-name-ref section) 8))
  (format stream "    Virtual size: ~A~%" (pe-section-header-virtual-size section))
  (format stream "    Virtual address: ~A 0x~x~%" (pe-section-header-virtual-address section) (pe-section-header-virtual-address section))
  (format stream "    Raw data: ~A @ ~a 0x~x~%" (pe-section-header-size-of-raw-data section) (pe-section-header-pointer-raw-data section) (pe-section-header-pointer-raw-data section))
  (format stream "    Relocs: ~A @ 0x~x~%" (pe-section-header-number-relocs section) (pe-section-header-pointer-to-relocs section))
  (format stream "    Line numbers: ~A @ 0x~x~%" (pe-section-header-number-line-numbers section) (pe-section-header-pointer-to-line-numbers section))
  (format stream "    Characteristics: 0x~x~%" (pe-section-header-characteristics section))
  (if (string= (ptr-read-string (pe-section-header-name-ref section) 8)
               ".idata")
      (print-import-table (pe-section-header-pointer-raw-data section)
                          ptr-size
                          stream
                          (+ image-base
                             (- (pe-section-header-virtual-address section)
                                (pe-section-header-pointer-raw-data section))))))

(defun print-each-section (image-base section num-sections ptr-size stream &optional (n 0))
  (print-section image-base section n ptr-size stream)
  (if (< n num-sections)
      (print-each-section image-base
                          (+ section (pe-section-header-size))
                          num-sections
                          ptr-size
                          stream
                          (+ n 1))))

(defun print-sections (image-base section-start num-sections ptr-size &optional (stream *standard-output*))
  (format stream "~%~A sections from 0x~x~%" num-sections section-start)
  (print-each-section image-base section-start num-sections ptr-size stream))

(defun print-pe-file (ptr &optional (stream *standard-output*))
  (let* ((dos-header ptr)
         (pe-header (+ ptr (exe-header-pe-header-offset dos-header)))
         (optional-header (+ pe-header (pe-header-size)))
         (section-start (+ optional-header (pe-header-size-optional-header pe-header)))
         (pe32+ (pe32+? ptr))
         (ptr-size (if pe32+ repl::*SIZEOF_ULONG64* repl::*SIZEOF_ULONG*)))
    (print-dos-header dos-header stream)
    (print-pe-header pe-header stream)
    (print-optional-header optional-header stream)
    (print-sections ptr section-start (pe-header-number-sections pe-header) ptr-size stream)))

(defun load-pe-file (path offset)
  (let* ((size (repl::ptr-read-file path offset))
         (pe-header (repl::ptr-read-ulong (+ offset PE-OFFSET-OFFSET)))
         (optional-header (+ pe-header (pe-header-size)))
         (section-start (+ optional-header (pe-header-size-optional-header pe-header))))
    (values pe-header
            optional-header
            section-start
            size)))
