; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs -filetype=obj < %s \
; RUN:   -o /dev/null 2>&1
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s | FileCheck %s

define void @relax_bcc(i1 %a) {
; CHECK-LABEL: relax_bcc:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    tsti a0, 1
; CHECK-NEXT:    bnz .LBB0_1
; CHECK-NEXT:    j .LBB0_2
; CHECK-NEXT:  .LBB0_1: ; %iftrue
; CHECK-NEXT:    ;APP
; CHECK-NEXT:    .zero 254
; CHECK-NEXT:    ;NO_APP
; CHECK-NEXT:  .LBB0_2: ; %tail
; CHECK-NEXT:    jalr ra, 0
  br i1 %a, label %iftrue, label %tail

iftrue:
  call void asm sideeffect ".space 254", ""()
  br label %tail

tail:
  ret void
}


define i16 @relax_jal(i1 %a) {
; CHECK-LABEL: relax_jal:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    tsti a0, 1
; CHECK-NEXT:    bnz .LBB1_1
; CHECK-NEXT:  ; %bb.3:
; CHECK-NEXT:    lui a2, .LBB1_2
; CHECK-NEXT:    jalr a2, .LBB1_2
; CHECK-NEXT:  .LBB1_1: ; %iftrue
; CHECK-NEXT:    ;APP
; CHECK-NEXT:    ;NO_APP
; CHECK-NEXT:    ;APP
; CHECK-NEXT:    .zero 1022
; CHECK-NEXT:    ;NO_APP
; CHECK-NEXT:    lli a0, 1
; CHECK-NEXT:    jalr ra, 0
; CHECK-NEXT:  .LBB1_2: ; %jmp
; CHECK-NEXT:    ;APP
; CHECK-NEXT:    ;NO_APP
; CHECK-NEXT:    lli a0, 1
; CHECK-NEXT:    jalr ra, 0
  br i1 %a, label %iftrue, label %jmp

jmp:
  call void asm sideeffect "", ""()
  br label %tail

iftrue:
  call void asm sideeffect "", ""()
  br label %space

space:
  call void asm sideeffect ".space 1022", ""()
  br label %tail

tail:
  ret i16 1
}
