; RUN: llvm-mc %s -triple=vc16 -show-encoding \
; RUN:     | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc -filetype=obj -triple vc16 < %s \
# RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INST %s

; CHECK-INST: addi sp, 2
; CHECK: encoding: [0x5f,0x02]
addi sp, 2
; CHECK-INST: andi a2, -2
; CHECK: encoding: [0xff,0xf6]
andi a2, -2
; CHECK-INST: ori a1, -16
; CHECK: encoding: [0x3f,0xa8]
ori a1, -16
; CHECK-INST: andi sp, 15
; CHECK: encoding: [0xff,0x47]
andi sp, 15


; CHECK-INST: add ra, sp
; CHECK: encoding: [0x37,0x08]
add ra, sp
; CHECK-INST: adc a1, a0
; CHECK: encoding: [0x37,0x6c]
adc a1, a0
; CHECK-INST: sub a0, a1
; CHECK: encoding: [0x77,0x25]
sub a0, a1
; CHECK-INST: sll t0, s0
; CHECK: encoding: [0x37,0xba]
sll t0, s0
; CHECK-INST: and a0, a0
; CHECK: encoding: [0xf7,0xa4]
and a0, a0
; CHECK-INST: xor a2, t0
; CHECK: encoding: [0xb7,0x37]
xor a2, t0
; CHECK-INST: sbc a1, ra
; CHECK: encoding: [0x77,0x69]
sbc a1, ra
; CHECK-INST: srl a0, a0
; CHECK: encoding: [0xb7,0xa4]
srl a0, a0
; CHECK-INST: sra a0, t0
; CHECK: encoding: [0xb7,0x67]
sra a0, t0
; CHECK-INST: or t0, ra
; CHECK: encoding: [0x37,0xf9]
or x7, ra
; CHECK-INST: not a0, s0
; CHECK: encoding: [0xb7,0xe2]
not a0, s0

