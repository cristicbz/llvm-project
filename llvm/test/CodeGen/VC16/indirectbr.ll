; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

define i16 @indirectbr(i8* %target) nounwind {
; VC16I-LABEL: indirectbr:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi s1, 4
; VC16I-NEXT:    jalr t0, a0, 0
; VC16I-NEXT:  .LBB0_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}

define i16 @indirectbr_with_offset(i8* %a) nounwind {
; VC16I-LABEL: indirectbr_with_offset:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi s1, 4
; VC16I-NEXT:    jalr t0, a0, 20
; VC16I-NEXT:  .LBB1_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  %target = getelementptr inbounds i8, i8* %a, i16 20
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}
