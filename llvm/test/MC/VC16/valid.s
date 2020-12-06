; RUN: llvm-mc %s -triple=vc16 -show-encoding \
; RUN:     | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc -filetype=obj -triple vc16 < %s \
# RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INST %s

; CHECK-INST: addi sp, 2
; CHECK: encoding: [0x5f,0x3a]
addi sp, 2
; CHECK-INST: andi r2, -2
; CHECK: encoding: [0xff,0xd6]
andi r2, -2
; CHECK-INST: xori r1, -7
; CHECK: encoding: [0x7f,0xc9]
xori r1, -7
; CHECK-INST: ori r1, -16
; CHECK: encoding: [0x3f,0x88]
ori r1, -16
; CHECK-INST: andi sp, 15
; CHECK: encoding: [0xff,0x7f]
andi sp, 15
; CHECK-INST: andi r2, 15
; CHECK: encoding: [0xff,0x57]
andi r2, 15


; CHECK-INST: add ra, sp
; CHECK: encoding: [0x37,0x37]
add ra, sp
; CHECK-INST: adc r1, r0
; CHECK: encoding: [0x37,0x48]
adc r1, r0
; CHECK-INST: sub r0, r1
; CHECK: encoding: [0xb7,0x01]
sub r0, r1
; CHECK-INST: sll r5, r3
; CHECK: encoding: [0x77,0x2b]
sll r5, r3
; CHECK-INST: and r0, r0
; CHECK: encoding: [0x37,0x80]
and r0, r0
; CHECK-INST: xor r2, r5
; CHECK: encoding: [0x77,0x95]
xor r2, r5
; CHECK-INST: sbb r1, ra
; CHECK: encoding: [0xb7,0x4e]
sbb r1, ra
; CHECK-INST: srl r0, r0
; CHECK: encoding: [0xf7,0x00]
srl r0, r0
; CHECK-INST: sra r0, r5
; CHECK: encoding: [0xf7,0xc5]
sra r0, r5
; CHECK-INST: or sp, ra
; CHECK: encoding: [0xb7,0xbe]
or r7, ra
; CHECK-INST: not r0, r3
; CHECK: encoding: [0xf7,0x83]
not r0, r3

