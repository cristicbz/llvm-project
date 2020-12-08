; RUN: llvm-mc %s -triple=vc16 -show-encoding \
; RUN:     | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc -filetype=obj -triple vc16 < %s \
# RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INST %s

; CHECK-INST: addi sp, 2
; CHECK: encoding: [0x5f,0x3a]
addi sp, 2
; CHECK-INST: andi a2, -2
; CHECK: encoding: [0xff,0xd6]
andi a2, -2
; CHECK-INST: xori a1, -7
; CHECK: encoding: [0x7f,0xc9]
xori a1, -7
; CHECK-INST: ori a1, -16
; CHECK: encoding: [0x3f,0x88]
ori a1, -16
; CHECK-INST: andi sp, 15
; CHECK: encoding: [0xff,0x7f]
andi sp, 15
; CHECK-INST: andi a2, 15
; CHECK: encoding: [0xff,0x57]
andi a2, 15


; CHECK-INST: add ra, sp
; CHECK: encoding: [0x37,0x37]
add ra, sp
; CHECK-INST: adc a1, a0
; CHECK: encoding: [0x37,0x48]
adc a1, a0
; CHECK-INST: sub a0, a1
; CHECK: encoding: [0xb7,0x01]
sub a0, a1
; CHECK-INST: sll t0, s0
; CHECK: encoding: [0x77,0x2b]
sll t0, s0
; CHECK-INST: and a0, a0
; CHECK: encoding: [0x37,0x80]
and a0, a0
; CHECK-INST: xor a2, t0
; CHECK: encoding: [0x77,0x95]
xor a2, t0
; CHECK-INST: sbb a1, ra
; CHECK: encoding: [0xb7,0x4e]
sbb a1, ra
; CHECK-INST: srl a0, a0
; CHECK: encoding: [0xf7,0x00]
srl a0, a0
; CHECK-INST: sra a0, t0
; CHECK: encoding: [0xf7,0xc5]
sra a0, t0
; CHECK-INST: or sp, ra
; CHECK: encoding: [0xb7,0xbe]
or r7, ra
; CHECK-INST: not a0, s0
; CHECK: encoding: [0xf7,0x83]
not a0, s0

