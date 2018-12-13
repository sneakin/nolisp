(in-package :repl-windows)

(require "runtime/defstruct")
(require "defenum")

;;; EXE header info from:
;;;   https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format
;;;   https://blog.kowalczyk.info/articles/pefileformat.html
;;;   http://www.delorie.com/djgpp/doc/exe/

(defconstant EXE-SIGNATURE '(#\M #\Z))
(defconstant EXE-SIGNATURE-SHORT (apply #'repl::make-ushort (mapcar #'char-code EXE-SIGNATURE)))

(repl:repl-defstruct exe-header
  ((signature :type :ushort)
   (last-block-size :type :ushort)
   (blocks-in-file :type :ushort) ;; a block = 512 bytes
   (num-relocs :type :ushort)
   (header-paragraphs :type :ushort) ;; a paragraph = 16 bytes
   (min-extra-paragraphs :type :ushort)
   (max-extra-paragraphs :type :ushort)
   (ss :type :ushort)
   (sp :type :ushort)
   (checksum :type :ushort)
   (ip :type :ushort)
   (cs :type :ushort)
   (reloc-table-offset :type :ushort)
   (overlay-number :type :ushort)
   (reserved :type :ushort :size 4)
   (oem-id :type :ushort)
   (oem-info :type :ushort)
   (reserved-2 :type :ushort :size 10)
   (pe-header-offset :type :ulong)))

(repl:repl-defstruct exe-reloc-entry
  ((offset :type :ushort)
   (segment :type :ushort)))

(defun exe-data-start (ptr)
  (* 16 (exe-header-header-paragraphs ptr)))

(defun exe-data-start-by-blocks (ptr)
  (* 512 (exe-header-blocks-in-file ptr)))

(defun exe-last-data-byte (ptr)
  (let ((n (* 512 (exe-header-blocks-in-file ptr))))
    (if (> (exe-header-last-block-size ptr) 0)
        (- n (- 512 (exe-header-last-block-size ptr)))
        n)))

(defconstant PE-SIGNATURE '(#\P #\E #\Nul #\Nul))
(defconstant PE-SIGNATURE-LONG (apply #'repl::make-ulong (mapcar #'char-code PE-SIGNATURE)))
(defconstant PE-OFFSET-OFFSET 60)

(repl::defenum PE-IMAGE-MACHINE
  (UNKNOWN   #x0) ; The contents of this field are assumed to be applicable to any machine type 
  (AM33      #x1d3) ; Matsushita AM33 
  (AMD64     #x8664) ; x64 
  (ARM       #x1c0) ; ARM little endian 
  (ARM64     #xaa64) ; ARM64 little endian 
  (ARMNT     #x1c4) ; ARM Thumb-2 little endian 
  (EBC       #xebc) ; EFI byte code 
  (I386      #x14c) ; Intel 386 or later processors and compatible processors 
  (IA64      #x200) ; Intel Itanium processor family 
  (M32R      #x9041) ; Mitsubishi M32R little endian 
  (MIPS16    #x266) ; MIPS16 
  (MIPSFPU   #x366) ; MIPS with FPU 
  (MIPSFPU16 #x466) ; MIPS16 with FPU 
  (POWERPC   #x1f0) ; Power PC little endian 
  (POWERPCFP #x1f1) ; Power PC with floating point support 
  (R4000     #x166) ; MIPS little endian 
  (RISCV32   #x5032) ; RISC-V 32-bit address space 
  (RISCV64   #x5064) ; RISC-V 64-bit address space 
  (RISCV128  #x5128) ; RISC-V 128-bit address space 
  (SH3       #x1a2) ; Hitachi SH3 
  (SH3DSP    #x1a3) ; Hitachi SH3 DSP 
  (SH4       #x1a6) ; Hitachi SH4 
  (SH5       #x1a8) ; Hitachi SH5 
  (THUMB     #x1c2) ; Thumb 
  (WCEMIPSV2 #x169) ; MIPS little-endian WCE v2
  (BACAW     #x6263) ; Bacaw VM
)

(repl::defenum PE-IMAGE-SUBSYS
  (UNKNOWN                   0) ; An unknown subsystem 
  (NATIVE                    1) ; Device drivers and native Windows processes 
  (WINDOWS-GUI               2) ; The Windows graphical user interface (GUI) subsystem
  (WINDOWS-CUI               3) ; The Windows character subsystem 
  (OS2-CUI                   5) ; The OS/2 character subsystem 
  (POSIX-CUI                 7) ; The Posix character subsystem 
  (NATIVE-WINDOWS            8) ; Native Win9x driver 
  (WINDOWS-CE-GUI            9) ; Windows CE 
  (EFI-APPLICATION           10) ; An Extensible Firmware Interface (EFI) application 
  (EFI-BOOT-SERVICE-DRIVER   11) ; An EFI driver with boot services 
  (EFI-RUNTIME-DRIVER        12) ; An EFI driver with run-time services 
  (EFI-ROM                   13) ; An EFI ROM image 
  (XBOX                      14) ; XBOX 
  (WINDOWS-BOOT-APPLICATION  16) ; Windows boot application.
)

(repl::defenum PE-IMAGE-DLL
  (HIGH-ENTROPY-VA #x002) ; Image can handle a high entropy 64-bit virtual address space. 
  (DYNAMIC-BASE #x004) ; DLL can be relocated at load time. 
  (FORCE-INTEGRITY #x008) ; Code Integrity checks are enforced. 
  (NX-COMPAT #x010) ; Image is NX compatible. 
  (NO-ISOLATION #x020) ; Isolation aware, but do not isolate the image. 
  (NO-SEH #x040) ; Does not use structured exception (SE) handling. No SE handler may be called in this image. 
  (NO-BIND #x080) ; Do not bind the image. 
  (APPCONTAINER #x100) ; Image must execute in an AppContainer. 
  ( WDM-DRIVER #x200) ; A WDM driver. 
  (GUARD-CF #x400) ; Image supports Control Flow Guard. 
  (TERMINAL-SERVER-AWARE #x800) ; Terminal Server aware.
)

(repl::defenum PE-IMAGE-FILE
  (RELOCS-STRIPPED #x0001) 
  (EXECUTABLE-IMAGE #x0002) 
  (LINE-NUMS-STRIPPED #x0004) 
  (LOCAL-SYMS-STRIPPED #x0008) 
  (AGGRESSIVE-WS-TRIM #x0010) 
  (LARGE-ADDRESS-AWARE #x0020) 
  (BYTES-RESERVED #x0040)
  (BYTES-REVERSED-LO #x0080) 
  (32BIT-MACHINE #x0100) 
  (DEBUG-STRIPPED #x0200) 
  (REMOVABLE-RUN-FROM-SWAP #x0400) 
  (NET-RUN-FROM-SWAP #x0800) 
  (SYSTEM #x1000) 
  (DLL #x2000) 
  (UP-SYSTEM-ONLY #x4000) 
  (BYTES-REVERSED-HI #x8000)
)

(defconstant PE-OPTIONAL-HEADER-PE32 #x10B)
(defconstant PE-OPTIONAL-HEADER-PE32+ #x20B)

(repl::defenum PE-DATA-DIRECTORY
  (EXPORT-TABLE 0)
  (IMPORT-TABLE 1)
  (RESOURCE-TABLE 2)
  (EXCEPTION-TABLE 3)
  (CERTIFICATE-TABLE 4)
  (BASE-RELOC-TABLE 5)
  (DEBUG 6)
  (ARCH 7)
  (GLOBAL-PTR 8)
  (TLS-TABLE 9)
  (LOAD-CONFIG-TABLE 10)
  (BOUND-IMPORT 11)
  (IAT 12)
  (DELAY-IMPORT-DESC 13)
  (CLR-RUNTIME-HEADER 14)
  (RESERVED 15)
)

(repl::defenum PE-IMAGE-FILE
    (RELOCS-STRIPPED                  #x0001) ; Image only, Windows CE, and Microsoft Windows NT and later. This indicates that the file does not contain base relocations and must therefore be loaded at its preferred base address. If the base address is not available, the loader reports an error. The default behavior of the linker is to strip base relocations from executable (EXE) files. 
  (EXECUTABLE-IMAGE                   #x0002) ; Image only. This indicates that the image file is valid and can be run. If this flag is not set, it indicates a linker error. 
  (LINE-NUMS-STRIPPED                 #x0004) ; COFF line numbers have been removed. This flag is deprecated and should be zero. 
  (LOCAL-SYMS-STRIPPED                #x0008) ; COFF symbol table entries for local symbols have been removed. This flag is deprecated and should be zero. 
  (AGGRESSIVE-WS-TRIM                 #x0010) ; Obsolete. Aggressively trim working set. This flag is deprecated for Windows 2000 and later and must be zero. 
  (LARGE-ADDRESS-AWARE     #x0020) ; Application can handle > 2-GB addresses. 
  (BYTES-REVERSED-LO       #x0080) ; Little endian: the least significant bit (LSB) precedes the most significant bit (MSB) in memory. This flag is deprecated and should be zero. 
  (32BIT-MACHINE           #x0100) ; Machine is based on a 32-bit-word architecture. 
  (DEBUG-STRIPPED          #x0200) ; Debugging information is removed from the image file. 
  (REMOVABLE-RUN-FROM-SWAP #x0400) ; If the image is on removable media, fully load it and copy it to the swap file. 
  (NET-RUN-FROM-SWAP       #x0800) ; If the image is on network media, fully load it and copy it to the swap file. 
  (SYSTEM                  #x1000) ; The image file is a system file, not a user program. 
  (DLL                     #x2000) ; The image file is a dynamic-link library (DLL). Such files are considered executable files for almost all purposes, although they cannot be directly run. 
  (UP-SYSTEM-ONLY          #x4000) ; The file should be run only on a uniprocessor machine. 
  (BYTES-REVERSED-HI       #x8000) ; Big endian: the MSB precedes the LSB in memory. This flag is deprecated and should be zero.
  )

(repl:repl-defstruct pe-header
 ((signature :type :ulong)
  (machine :type :ushort)
  (number-sections :type :ushort)
  (timedate-stamp :type :ulong)
  (symbol-table-pointer :type :ulong)
  (number-symbols :type :ulong)
  (size-optional-header :type :ushort)
  (characteristics :type :ushort)))

(repl:repl-defstruct pe-data-directory-entry
  ((rva :type :ulong)
   (entry-size :type :ulong)))

(repl:repl-defstruct pe-optional-header
 ((magic :type :ushort)
  (major-linker-version :type :ubyte)
  (minor-linker-version :type :ubyte)
  (size-code :type :ulong)
  (size-initialized-data :type :ulong)
  (size-uninitialized-data :type :ulong)
  (address-entry-point :type :ulong)
  (base-of-code :type :ulong)))

(defun pe32+? (ptr)
  (eq PE-OPTIONAL-HEADER-PE32+
      (pe-optional-header-magic (exe-header-pe-header-offset ptr))))

(repl:repl-defstruct pe-optional-header-pe32
 ((base-of-data :type :ulong)
  ;; windows specific
  (image-base :type :ulong)
  (section-alignment :type :ulong)
  (file-alignment :type :ulong)
  (major-os-version :type :ushort)
  (minor-os-version :type :ushort)
  (major-image-version :type :ushort)
  (minor-image-version :type :ushort)
  (major-subsystem-version :type :ushort)
  (minor-subsystem-version :type :ushort)
  (win32-version :type :ulong)
  (size-image :type :ulong)
  (size-headers :type :ulong)
  (checksum :type :ulong)
  (subsystem :type :ushort)
  (dll-characteristics :type :ushort)
  (size-stack-reserve :type :ulong)
  (size-stack-commit :type :ulong)
  (size-heap-reserve :type :ulong)
  (size-heap-commit :type :ulong)
  (loader-flags :type :ulong)
  (number-directories :type :ulong)
  (directories :type pe-data-directory-entry :size 16)))

(repl:repl-defstruct pe-optional-header-pe32+
 (;; windows specific
  (image-base :type :ulong64)
  (section-alignment :type :ulong)
  (file-alignment :type :ulong)
  (major-os-version :type :ushort)
  (minor-os-version :type :ushort)
  (major-image-version :type :ushort)
  (minor-image-version :type :ushort)
  (major-subsystem-version :type :ushort)
  (minor-subsystem-version :type :ushort)
  (win32-version :type :ulong)
  (size-image :type :ulong)
  (size-headers :type :ulong)
  (checksum :type :ulong)
  (subsystem :type :ushort)
  (dll-characteristics :type :ushort)
  (size-stack-reserve :type :ulong64)
  (size-stack-commit :type :ulong64)
  (size-heap-reserve :type :ulong64)
  (size-heap-commit :type :ulong64)
  (loader-flags :type :ulong)
  (number-directories :type :ulong)
  (directories :type pe-data-directory-entry :size 16)
  ))

(repl::defenum PE-SECTION
  (TYPE-NO-PAD             #x00000008) ; The section should not be padded to the next boundary. This flag is obsolete and is replaced by ALIGN-1BYTES. This is valid only for object files. 
  (CNT-CODE                #x00000020) ; The section contains executable code. 
  (CNT-INITIALIZED-DATA    #x00000040) ; The section contains initialized data. 
  (CNT-UNINITIALIZED-DATA  #x00000080) ; The section contains uninitialized data. 
  (LNK-OTHER               #x00000100) ; Reserved for future use.
  (LNK-INFO                #x00000200) ; The section contains comments or other information. The .drectve section has this type. This is valid for object files only.
  (LNK-REMOVE              #x00000800) ; The section will not become part of the image. This is valid only for object files.
  (LNK-COMDAT              #x00001000) ; The section contains COMDAT data. For more information, see COMDAT Sections (Object Only). This is valid only for object files.
  (GPREL                   #x00008000) ; The section contains data referenced through the global pointer (GP).
  (MEM-PURGEABLE           #x00020000) ; Reserved for future use.
  (MEM-16BIT               #x00020000) ; Reserved for future use.
  (MEM-LOCKED              #x00040000) ; Reserved for future use.
  (MEM-PRELOAD             #x00080000) ; Reserved for future use.
  (ALIGN-1BYTES            #x00100000) ; Align data on a 1-byte boundary. Valid only for object files.
  (ALIGN-2BYTES            #x00200000) ; Align data on a 2-byte boundary. Valid only for object files.
  (ALIGN-4BYTES            #x00300000) ; Align data on a 4-byte boundary. Valid only for object files.
  (ALIGN-8BYTES            #x00400000) ; Align data on an 8-byte boundary. Valid only for object files.
  (ALIGN-16BYTES           #x00500000) ; Align data on a 16-byte boundary. Valid only for object files.
  (ALIGN-32BYTES           #x00600000) ; Align data on a 32-byte boundary. Valid only for object files.
  (ALIGN-64BYTES           #x00700000) ; Align data on a 64-byte boundary. Valid only for object files.
  (ALIGN-128BYTES          #x00800000) ; Align data on a 128-byte boundary. Valid only for object files.
  (ALIGN-256BYTES          #x00900000) ; Align data on a 256-byte boundary. Valid only for object files.
  (ALIGN-512BYTES          #x00A00000) ; Align data on a 512-byte boundary. Valid only for object files.
  (ALIGN-1024BYTES         #x00B00000) ; Align data on a 1024-byte boundary. Valid only for object files.
  (ALIGN-2048BYTES         #x00C00000) ; Align data on a 2048-byte boundary. Valid only for object files.
  (ALIGN-4096BYTES         #x00D00000) ; Align data on a 4096-byte boundary. Valid only for object files.
  (ALIGN-8192BYTES         #x00E00000) ; Align data on an 8192-byte boundary. Valid only for object files.
  (LNK-NRELOC-OVFL         #x01000000) ; The section contains extended relocations.
  (MEM-DISCARDABLE         #x02000000) ; The section can be discarded as needed.
  (MEM-NOT-CACHED          #x04000000) ; The section cannot be cached.
  (MEM-NOT-PAGED           #x08000000) ; The section is not pageable.
  (MEM-SHARED              #x10000000) ; The section can be shared in memory.
  (MEM-EXECUTE             #x20000000) ; The section can be executed as code. 
  (MEM-READ                #x40000000) ; The section can be read. 
  (MEM-WRITE               #x80000000) ; The section can be written to.
)

(repl:repl-defstruct pe-section-header
  ((name :type :byte :size 8)
   (virtual-size :type :ulong)
   (virtual-address :type :ulong)
   (size-of-raw-data :type :ulong)
   (pointer-raw-data :type :ulong)
   (pointer-to-relocs :type :ulong)
   (pointer-to-line-numbers :type :ulong)
   (number-relocs :type :ushort)
   (number-line-numbers :type :ushort)
   (characteristics :type :ulong)))

(defun set-pe-section-header-name (header name)
  (repl::ptr-write-string name (pe-section-header-name-ref header) 8))

(repl:repl-defstruct pe-import-table-entry
                     ((lookup-rva :type :ulong)
                      (timedate-stamp :type :ulong)
                      (forwarder-chain :type :ulong)
                      (name-rva :type :ulong)
                      (address-rva :type :ulong)))

(repl:repl-defstruct pe-hint-name
                     ((hint :type :ushort)
                      (name :type :byte :size 256)))

(defun set-pe-hint-name-name (import name)
  (repl::ptr-write-ubyte 0 (repl::ptr-write-string name (pe-hint-name-name-ref import))))

(defun pe-hint-name-name-string (entry)
  (ptr-read-string (pe-hint-name-name-ref entry)))

(defun pe-hint-name-true-size (entry)
  (repl::align-bytes (+ repl::*SIZEOF_SHORT*
                        1
                        (length (pe-hint-name-name-string entry)))
                     2))

(defun pe-hint-name-by-ordinal? (lookup-entry ptr-size)
  (< 0 (logand (pe-hint-name-hint lookup-entry)
          (if (= ptr-size repl::*SIZEOF_LONG*)
              #x80000000
              #x80000000000000))))

(defun pe-hint-name-string (st)
  (ptr-read-string (pe-hint-name-name-ref st)))

(defun pe-hint-name-real-size (st)
  (repl::align-bytes (+ repl::*SIZEOF_SHORT* (length (pe-hint-name-string st)))
                     2))
