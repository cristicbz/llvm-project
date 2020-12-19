; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

declare void @notdead(i8*)

; These tests must ensure the stack pointer is restored using the frame
; pointer

define void @simple_alloca(i16 %n) nounwind {
; VC16I-LABEL: simple_alloca:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    addi a0, 1
; VC16I-NEXT:    andi a0, -2
; VC16I-NEXT:    mv a2, sp
; VC16I-NEXT:    sub a2, a0
; VC16I-NEXT:    mv sp, a2
; VC16I-NEXT:    lui ra, %his(notdead)
; VC16I-NEXT:    addi ra, %lo(notdead)
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:    jalr ra, 0
; VC16I-NEXT:    mv sp, s0
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = alloca i8, i16 %n
  call void @notdead(i8* %1)
  ret void
}


declare i8* @llvm.stacksave()
declare void @llvm.stackrestore(i8*)

define void @scoped_alloca(i16 %n) nounwind {
; VC16I-LABEL: scoped_alloca:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -6
; VC16I-NEXT:    sw s1, 4(sp)
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 6
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi a0, 1
; VC16I-NEXT:    andi a0, -2
; VC16I-NEXT:    mv a2, sp
; VC16I-NEXT:    sub a2, a0
; VC16I-NEXT:    mv sp, a2
; VC16I-NEXT:    lui ra, %his(notdead)
; VC16I-NEXT:    addi ra, %lo(notdead)
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:    jalr ra, 0
; VC16I-NEXT:    mv sp, s1
; VC16I-NEXT:    mv sp, s0
; VC16I-NEXT:    addi sp, -6
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    lw s1, 4(sp)
; VC16I-NEXT:    addi sp, 6
; VC16I-NEXT:    jalr ra, 0
  %sp = call i8* @llvm.stacksave()
  %addr = alloca i8, i16 %n
  call void @notdead(i8* %addr)
  call void @llvm.stackrestore(i8* %sp)
  ret void
}
