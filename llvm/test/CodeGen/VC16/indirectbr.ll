; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

define i16 @indirectbr(i8* %target) nounwind {
; VC16I-LABEL: indirectbr:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    jr a0
; VC16I-NEXT:  .LBB0_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    ret
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}

define i16 @indirectbr_with_offset(i8* %a) nounwind {
; VC16I-LABEL: indirectbr_with_offset:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    jalr a0, 10
; VC16I-NEXT:  .LBB1_1: ; %test_label
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    ret
  %target = getelementptr inbounds i8, i8* %a, i16 10
  indirectbr i8* %target, [label %test_label]
test_label:
  br label %ret
ret:
  ret i16 0
}
