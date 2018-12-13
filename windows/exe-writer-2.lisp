(in-package :repl-windows)

(require "runtime/defstruct")
(require "windows/exe-file")
(require "windows/dos-stub")

(defun file-excursion (stream offset fn)
  (let ((pos (file-position stream)))
    (unwind-protect
         (progn
           (file-position stream offset)
           (funcall fn))
      (file-position stream pos))))

(defconstant UNIX-EPOCH (encode-universal-time 0 0 0 1 1 1970))

(defun get-unix-time ()
  (- (get-universal-time)
     UNIX-EPOCH))

(defun write-ulong (value stream)
  (repl::with-allocation (b repl::*SIZEOF_LONG*)
    (repl::ptr-write-ulong value b)
    (write-sequence (repl::ptr-read-array b repl::*SIZEOF_LONG*)
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

;;; PE Writer

(repl::repl-defstruct pe-writer-state
  ((next-mem-page :type :pointer)
   (next-file-page :type :pointer)
   (segment-headers :type :ulong)
   (next-segment :type :ulong)
   (num-segments :type :ulong)
   (image-base :type :ulong)
   (mem-alignment :type :ulong)
   (file-alignment :type :ulong)
   (pe-header-offset :type :ulong)
   (pe-opt-header-offset :type :ulong)
   (pe32-header-offset :type :ulong)
   (code-segment :type :ulong)
   (code-size :type :ulong)
   (data-size :type :ulong)
   (pe32+ :type :ulong)
   ))

(defconstant DEFAULT-IMAGE-BASE (* 4 1024 1024))
(defconstant PAGE-SIZE 4096)
(defconstant DEFAULT-SECTION-ALIGNMENT 4096)
(defconstant DEFAULT-FILE-ALIGNMENT 512)

(defun pe-writer-state-init (pe num-segments &optional pe32+ (image-base DEFAULT-IMAGE-BASE) (page-alignment DEFAULT-SECTION-ALIGNMENT) (file-alignment DEFAULT-FILE-ALIGNMENT))
  (set-pe-writer-state-image-base pe image-base)
  (set-pe-writer-state-pe32+ pe pe32+)
  (set-pe-writer-state-next-mem-page pe image-base)
  (set-pe-writer-state-next-file-page pe 0)
  (set-pe-writer-state-segment-headers pe 0)
  (set-pe-writer-state-next-segment pe 0)
  (set-pe-writer-state-num-segments pe num-segments)
  (set-pe-writer-state-mem-alignment pe page-alignment)
  (set-pe-writer-state-file-alignment pe file-alignment)
  (set-pe-writer-state-pe-header-offset pe 0)
  (set-pe-writer-state-pe-opt-header-offset pe 0)
  (set-pe-writer-state-pe32-header-offset pe 0)
  )

(defun pe-writer-state-get-segment-header (exe &optional (n (pe-writer-state-next-segment exe)))
  (let ((offset (pe-writer-state-segment-headers exe)))
    (if (>= n (pe-writer-state-num-segments exe))
        (error 'too-many-segments :number n))
    (+ offset (* n (pe-section-header-size)))))

(defun pe-writer-state-next-segment-header (exe)
  (let* ((n (pe-writer-state-next-segment exe))
         (header (pe-writer-state-get-segment-header exe n)))
    (set-pe-writer-state-next-segment exe (+ 1 n))
    header))

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

;;; Legacy / DOS header and stub

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
      (write-aligned-padding stream 128))))

;;; Windows' PE header

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

;;; PE optional header

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

(defun write-empty-pe-optional-header (stream)
  (write-pe-optional-header stream 0 0 0 0 0))

(defun rewrite-pe-optional-header (stream offset cs code-size constants-size bss-size entry-point &optional (magic PE-OPTIONAL-HEADER-PE32))
  (file-excursion stream offset (lambda ()
                                  (write-pe-optional-header stream cs code-size constants-size bss-size entry-point magic))))

(defun patch-pe-optional-header (stream offset cs code-size constants-size bss-size entry-point &optional (magic PE-OPTIONAL-HEADER-PE32))
  (file-excursion stream offset (lambda ()
                                  (write-pe-optional-header stream cs code-size constants-size bss-size entry-point magic))))

;;; PE32 Header

(defun write-pe-optional-pe32-header (stream exe subsystem)
  (repl::with-allocation (header (pe-optional-header-pe32-size))
    (repl::ptr-set header (pe-optional-header-pe32-size) 0)
    (set-pe-optional-header-pe32-image-base header (pe-writer-state-image-base exe))
    (set-pe-optional-header-pe32-section-alignment header (pe-writer-state-mem-alignment exe))
    (set-pe-optional-header-pe32-file-alignment header (pe-writer-state-file-alignment exe))
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

(defun patch-pe32-header-long-inner (stream pe-header slot value)
  (file-position stream (+ pe-header (pe-optional-header-pe32-slot-offset slot)))
  (write-ulong value stream))

(defun patch-pe32-header-long (stream pe-header slot value)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32-header-long-inner stream pe-header slot value))))

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

;;; PE32+ Header

(defun write-pe-optional-pe32+-header (stream exe subsystem)
  (repl::with-allocation (header (pe-optional-header-pe32+-size))
    (repl::ptr-set header (pe-optional-header-pe32+-size) 0)
    (set-pe-optional-header-pe32+-image-base header (pe-writer-state-image-base exe))
    (set-pe-optional-header-pe32+-section-alignment header (pe-writer-state-mem-alignment exe))
    (set-pe-optional-header-pe32+-file-alignment header (pe-writer-state-file-alignment exe))
    (set-pe-optional-header-pe32+-major-os-version header 4)
    (set-pe-optional-header-pe32+-minor-os-version header 0)
    (set-pe-optional-header-pe32+-major-subsystem-version header 4)
    (set-pe-optional-header-pe32+-minor-subsystem-version header 0)
    (set-pe-optional-header-pe32+-size-image header 0) ; fix in post
    (set-pe-optional-header-pe32+-size-headers header 0)
    (set-pe-optional-header-pe32+-subsystem header subsystem)
    (set-pe-optional-header-pe32+-dll-characteristics header 0) ;; todo
    (set-pe-optional-header-pe32+-size-stack-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32+-size-stack-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32+-size-heap-reserve header (* 1024 1024))
    (set-pe-optional-header-pe32+-size-heap-commit header PAGE-SIZE)
    (set-pe-optional-header-pe32+-number-directories header 16)
    (write-sequence (ptr-read-array header (pe-optional-header-pe32+-size))
                    stream)))

(defun patch-pe32+-header-long-inner (stream pe-header slot value)
  (file-position stream (+ pe-header (pe-optional-header-pe32+-slot-offset slot)))
  (write-ulong value stream))

(defun patch-pe32+-header-long (stream pe-header slot value)
  (file-excursion stream pe-header (lambda ()
                                     (patch-pe32+-header-long-inner stream pe-header slot value))))

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

;;; Segment writing

(defun write-segment-header (stream name rva virtual-size raw-pointer raw-size characteristics)
  (repl::with-allocation (header (pe-section-header-size))
    (repl::ptr-zero header (pe-section-header-size))
    (set-pe-section-header-name header name)
    (set-pe-section-header-virtual-size header virtual-size)
    (set-pe-section-header-virtual-address header rva)
    (set-pe-section-header-size-of-raw-data header raw-size)
    (set-pe-section-header-pointer-raw-data header raw-pointer)
    (set-pe-section-header-characteristics header characteristics)
    (write-sequence (repl::ptr-read-array header (pe-section-header-size))
                    stream)
    ))

(defun rewrite-segment-header (stream segment-offset name rva virtual-size raw-pointer raw-size characteristics)
  (file-excursion stream segment-offset
                  (lambda ()
                    (write-segment-header stream name rva virtual-size raw-pointer raw-size characteristics))))

(defun write-empty-segment-header (stream name)
  (write-segment-header stream
                        (if name name "")
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
      (write-aligned-padding stream (pe-writer-state-file-alignment pe))))

(defun write-segment-data (exe stream data &optional (num-bytes (length data)))
  (let* ((aligned-size (repl::align-bytes num-bytes (pe-writer-state-file-alignment exe))))
      (write-sequence data stream :end num-bytes)
      (write-aligned-padding stream (pe-writer-state-file-alignment exe))
      aligned-size))

;;; External API

(defun pe-writer-state-inc-segment (exe stream size flags)
  (let ((mem-end (pe-writer-state-inc-mem-page exe size)))
    (set-pe-writer-state-next-file-page exe (file-position stream))
    (cond
      ((< 0 (logand flags PE-SECTION-CNT-CODE))
       (pe-writer-state-inc-code-size exe size))
      ((< 0 (logand flags (logior PE-SECTION-CNT-INITIALIZED-DATA
                                  PE-SECTION-CNT-UNINITIALIZED-DATA)))
       (pe-writer-state-inc-data-size exe size)))
    mem-end))


(defun mark-pe-segment (exe stream name start-pos real-size virtual-size flags)
  (let* ((section-offset (pe-writer-state-next-segment-header exe))
         (mem-page (pe-writer-state-next-mem-page exe)))
    ;; update segment header in the filee
    (rewrite-segment-header stream
                            section-offset
                            name
                            mem-page
                            virtual-size
                            (if real-size start-pos 0)
                            (if real-size real-size 0)
                            flags)
    ;; return segment start and size
    (values mem-page
            ;; more state updates
            (- (pe-writer-state-inc-segment exe stream (if real-size real-size virtual-size) flags)
               mem-page))))

(defun write-pe-segment (exe stream name data data-len flags)
  (let* ((pos (file-position stream))
         ;; write the data out
         (real-size (if data
                        (write-segment-data exe stream (repl::ptr-read-array data data-len) data-len))))
    (mark-pe-segment exe stream name pos real-size data-len flags)))


(defun with-pe-file (path num-segments pe32+ fn &optional (image-base DEFAULT-IMAGE-BASE))
  (repl::with-allocation (exe (pe-writer-state-size))
    (pe-writer-state-init exe num-segments pe32+ image-base)
    (with-open-file (stream path
                            :direction :output
                            :element-type '(unsigned-byte 8))
      ;; dos header
      (write-dos-header stream)
      ;; pe header
      (set-pe-writer-state-pe-header-offset exe (file-position stream))
      (write-pe-header stream
                       (if pe32+ PE-IMAGE-MACHINE-AMD64 PE-IMAGE-MACHINE-I386)
                       num-segments
                       pe32+
                       (logior PE-IMAGE-FILE-EXECUTABLE-IMAGE
                               (if pe32+ PE-IMAGE-FILE-LARGE-ADDRESS-AWARE 0)
                               PE-IMAGE-FILE-RELOCS-STRIPPED
                               PE-IMAGE-FILE-LINE-NUMS-STRIPPED
                               PE-IMAGE-FILE-LOCAL-SYMS-STRIPPED
                               PE-IMAGE-FILE-DEBUG-STRIPPED))
      ;;(write-pe-header f PE-IMAGE-MACHINE-BACAW 5)
      ;; optional header
      (set-pe-writer-state-pe-opt-header-offset exe (file-position stream))
      (write-empty-pe-optional-header stream)
      (set-pe-writer-state-pe32-header-offset exe (file-position stream))
      (if pe32+
          (write-pe-optional-pe32+-header stream exe PE-IMAGE-SUBSYS-WINDOWS-CUI)
          (write-pe-optional-pe32-header stream exe PE-IMAGE-SUBSYS-WINDOWS-CUI))
      ;; segment list followed by the segments' data
      (set-pe-writer-state-segment-headers exe (file-position stream))
      (write-empty-segment-headers exe stream num-segments)
      ;; store the header's offset to patch offsets
      (let ((header-end (file-position stream)))
        (set-pe-writer-state-next-file-page exe header-end)
        (set-pe-writer-state-next-mem-page exe (repl::align-bytes header-end (pe-writer-state-mem-alignment exe)))
        ;; call the callback
        (funcall fn exe stream)
        ;; patch sizes in headers
        (if pe32+
            (patch-pe32+-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-image (pe-writer-state-next-mem-page exe))
            (patch-pe32-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-image (pe-writer-state-next-mem-page exe)))
        (if pe32+
            (patch-pe32+-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-headers header-end)
            (patch-pe32-header-long stream (pe-writer-state-pe32-header-offset exe) 'size-headers header-end))
        (rewrite-pe-optional-header stream
                                    (pe-writer-state-pe-opt-header-offset exe)
                                    (pe-writer-state-code-segment exe)
                                    (pe-writer-state-code-size exe)
                                    (pe-writer-state-data-size exe)
                                    0 0
                                    (if pe32+
                                        PE-OPTIONAL-HEADER-PE32+
                                        PE-OPTIONAL-HEADER-PE32))
        (file-position stream)))))

