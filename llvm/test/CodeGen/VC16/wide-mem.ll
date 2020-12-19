; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

; Check load/store operations on values wider than what is natively supported

define i32 @load_i32(i32 *%a) nounwind {
; VC16I-LABEL: load_i32:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    lw a2, 0(a0)
; VC16I-NEXT:    lw a1, 2(a0)
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  %1 = load i32, i32* %a
  ret i32 %1
}

define i32 @load_i64(i64 *%a) nounwind {
; VC16I-LABEL: load_i64:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    lw a2, 6(a0)
; VC16I-NEXT:    lw a2, 4(a0)
; VC16I-NEXT:    lw a1, 2(a0)
; VC16I-NEXT:    lw a0, 0(a0)
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  %1 = load volatile i64, i64* %a
  %2 = trunc i64 %1 to i32
  ret i32 %2
}


@val32 = local_unnamed_addr global i32 2863311530, align 2

; TODO(cristicbz): codegen on this should be improved. It shouldn't be necessary
; to generate two addi
define i32 @load_i32_global() nounwind {
; VC16I-LABEL: load_i32_global:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    lui a2, %his(val32)
; VC16I-NEXT:    addi a2, %lo(val32)
; VC16I-NEXT:    lw a0, 0(a2)
; VC16I-NEXT:    lui a2, %his(val32+2)
; VC16I-NEXT:    addi a2, %lo(val32+2)
; VC16I-NEXT:    lw a1, 0(a2)
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  %1 = load i32, i32* @val32
  ret i32 %1
}
