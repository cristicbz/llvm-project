; RUN: llvm-mc -triple vc16 < %s -show-encoding  \
; RUN:     | FileCheck -check-prefix=CHECK-FIXUP %s
; RUN: llvm-mc -filetype=obj -triple vc16 < %s \
; RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INSTR %s
; RUN: llvm-mc -filetype=obj -triple=vc16 %s \
; RUN:     | llvm-readobj -r | FileCheck %s -check-prefix=CHECK-REL

; Checks that fixups that can be resolved within the same object file are
; applied correctly

.LBB0:
lui t0, %hiu(val)
; CHECK-FIXUP: fixup A - offset: 0, value: %hiu(val), kind: fixup_vc16_hi11u
; CHECK-INSTR: lui t0, 691

lw a0, %lo(val)(t0)
; CHECK-FIXUP: fixup A - offset: 0, value: %lo(val), kind: fixup_vc16_lo5_mw
; CHECK-INSTR: lw a0, 24(t0)
lea a1, t0, %lo(val)
; CHECK-FIXUP: fixup A - offset: 0, value: %lo(val), kind: fixup_vc16_lo5_rri5
; CHECK-INSTR: lea a1, t0, -8
addi a1, %lo(val)
; CHECK-FIXUP: fixup A - offset: 0, value: %lo(val), kind: fixup_vc16_lo5_ri5
; CHECK-INSTR: addi a1, -8
sb a0, %lo(val)(t0)
; CHECK-FIXUP: fixup A - offset: 0, value: %lo(val), kind: fixup_vc16_lo5_mb
; CHECK-INSTR: sb a0, 24(t0)

j .LBB0
; CHECK-FIXUP: fixup A - offset: 0, value: .LBB0, kind: fixup_vc16_jal
; CHECK-INSTR: j -10
jal t0, .LBB2
; CHECK-FIXUP: fixup A - offset: 0, value: .LBB2, kind: fixup_vc16_jal
; CHECK-INSTR: jal t0, 708
bz .LBB0
; CHECK-FIXUP: fixup A - offset: 0, value: .LBB0, kind: fixup_vc16_branch
; CHECK-INSTR: bz -14
blt .LBB1
; CHECK-FIXUP: fixup A - offset: 0, value: .LBB1, kind: fixup_vc16_branch
; CHECK-INSTR: blt 202

.fill 200

.LBB1:

.fill 500
nop
.LBB2:

.set val, 0x5678

; CHECK-REL-NOT: R_VC16
