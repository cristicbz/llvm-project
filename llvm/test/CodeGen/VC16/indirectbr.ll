; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

define i16 @indirectbr(i8* %target) nounwind {
; VC16I-LABEL: indirectbr:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    lea s0, sp, 4
; VC16I-NEXT:    jalr a0, 0
; VC16I-NEXT:  .LBB0_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    lea sp, sp, 4
; VC16I-NEXT:    jalr ra, 0
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}

define i16 @indirectbr_with_offset(i8* %a) nounwind {
; VC16I-LABEL: indirectbr_with_offset:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    lea s0, sp, 4
; VC16I-NEXT:    jalr a0, 10
; VC16I-NEXT:  .LBB1_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    lea sp, sp, 4
; VC16I-NEXT:    jalr ra, 0
  %target = getelementptr inbounds i8, i8* %a, i16 10
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}
