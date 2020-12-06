; RUN: llvm-mc %s -filetype=obj -triple=vc16 | llvm-readobj -h \
; RUN:     | FileCheck -check-prefix=VC16 %s

; VC16: Format: elf32-vc16
; VC16: Arch: vc16
; VC16: AddressSize: 32bit
; VC16: ElfHeader {
; VC16:   Ident {
; VC16:     Magic: (7F 45 4C 46)
; VC16:     Class: 32-bit (0x1)
; VC16:     DataEncoding: LittleEndian (0x1)
; VC16:     FileVersion: 1
; VC16:     OS/ABI: SystemV (0x0)
; VC16:     ABIVersion: 0
; VC16:   }
; VC16:   Type: Relocatable (0x1)
; VC16:   Machine: EM_VC16 (0xFE)
; VC16:   Version: 1
; VC16:   Flags [ (0x0)
; VC16:   ]
; VC16: }
