; RUN: llvm-mc %s -triple=vc16 -show-encoding \
; RUN:     | FileCheck -check-prefixes=CHECK,CHECK-INST %s
# RUN: llvm-mc -filetype=obj -triple vc16 < %s \
# RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INST %s

# CHECK-INST: lui s0, 0
# CHECK: encoding: [0x00,0x10]
lui s0, %his(2)
# CHECK-INST: lui a1, 1234
# CHECK: encoding: [0x48,0xab]
lui a1, (39488>>5)
# CHECK-INST: lui a1, 1234
# CHECK: encoding: [0x48,0xab]
lui a1, %hiu(39488)


# CHECK-INST: lw a0, 36(t0)
# CHECK: encoding: [0xd1,0xa7]
lw a0, 36(t0)

# CHECK-INST: sw t0, 44(a0)
# CHECK: encoding: [0xf2,0xbc]
sw t0, 44(a0)

# CHECK-INST: lb a0, 18(t0)
# CHECK: encoding: [0xd5,0xa7]
lb a0, 18(t0)

# CHECK-INST: sb t0, 22(a0)
# CHECK: encoding: [0xf6,0xbc]
sb t0, 22(a0)

# CHECK-INST: lw a0, 36(t0), xs
# CHECK: encoding: [0x51,0xa7]
lw a0, 36(t0), xs

# CHECK-INST: sw t0, 44(a0), xs
# CHECK: encoding: [0x72,0xbc]
sw t0, 44(a0), xs

# CHECK-INST: lb a0, 18(t0), xs
# CHECK: encoding: [0x55,0xa7]
lb a0, 18(t0), xs

# CHECK-INST: sb t0, 22(a0), xs
# CHECK: encoding: [0x76,0xbc]
sb t0, 22(a0), xs

# CHECK-INST: jal 1000
# CHECK: encoding: [0xa3,0x4f]
jal 1000

# CHECK-INST: jal -1024
# CHECK: encoding: [0x03,0x88]
jal -1024

# CHECK-INST: jal t0, 1022
# CHECK: encoding: [0xfb,0x7f]
jal t0, 1022

# CHECK-INST: jal t0, -1024
# CHECK: encoding: [0x03,0xb8]
jal t0, -1024

# CHECK-INST: jal t0, -1024
# CHECK: encoding: [0x03,0xb8]
jal t0, -1024

# CHECK-INST: j -1024
# CHECK: encoding: [0x03,0x80]
j -1024

# CHECK-INST: lea t0, a0, 15
# CHECK: encoding: [0xe7,0x7c]
lea t0, a0, 15

# CHECK-INST: lea t0, a0, -16
# CHECK: encoding: [0x07,0xbc]
lea t0, a0, -16

# CHECK-INST: lea t0, a0, 5
# CHECK: encoding: [0xa7,0x3c]
lea t0, a0, 5

# CHECK-INST: lea t0, a0, -4
# CHECK: encoding: [0x87,0xfc]
lea t0, a0, -4

# CHECK-INST: mv t0, a0
# CHECK: encoding: [0x07,0x3c]
mv t0, a0

# CHECK-INST: ret
# CHECK: encoding: [0x0f,0x08]
ret

# CHECK-INST: jr a0
# CHECK: encoding: [0x0f,0x20]
jr a0

# CHECK-INST: jalr a0, -16
# CHECK: encoding: [0x0f,0xa0]
jalr a0, -16

# CHECK-INST: jalr a0, 5
# CHECK: encoding: [0x0f,0x25]
jalr a0, 5

# CHECK-INST: bz -256
# CHECK: encoding: [0x2f,0x80]
bz -256

# CHECK-INST: bnz -256
# CHECK: encoding: [0x4f,0x80]
bnz -256

# CHECK-INST: bn -256
# CHECK: encoding: [0x6f,0x80]
bn -256

# CHECK-INST: bnn -256
# CHECK: encoding: [0x8f,0x80]
bnn -256

# CHECK-INST: blt -256
# CHECK: encoding: [0xaf,0x80]
blt -256

# CHECK-INST: bge -256
# CHECK: encoding: [0xcf,0x80]
bge -256

# CHECK-INST: bnc -256
# CHECK: encoding: [0xef,0x80]
bnc -256

# CHECK-INST: bz 86
# CHECK: encoding: [0x2f,0x2b]
bz 86

# CHECK-INST: bnz 86
# CHECK: encoding: [0x4f,0x2b]
bnz 86

# CHECK-INST: bn 86
# CHECK: encoding: [0x6f,0x2b]
bn 86

# CHECK-INST: bnn 86
# CHECK: encoding: [0x8f,0x2b]
bnn 86

# CHECK-INST: blt 86
# CHECK: encoding: [0xaf,0x2b]
blt 86

# CHECK-INST: bge 86
# CHECK: encoding: [0xcf,0x2b]
bge 86

# CHECK-INST: bnc 86
# CHECK: encoding: [0xef,0x2b]
bnc 86

# CHECK-INST: mul s0, s1
# CHECK: encoding: [0x17,0xd3]
mul s0, s1

# CHECK-INST: mlhu s0, s1
# CHECK: encoding: [0x57,0xd3]
mlhu s0, s1

# CHECK-INST: mlhs s0, s1
# CHECK: encoding: [0x97,0xd3]
mlhs s0, s1

# CHECK-INST: neg s0, s1
# CHECK: encoding: [0xd7,0xd3]
neg s0, s1

# CHECK-INST: add s0, s1
# CHECK: encoding: [0x37,0x13]
add s0, s1

# CHECK-INST: sub s0, s1
# CHECK: encoding: [0x77,0x13]
sub s0, s1

# CHECK-INST: xor s0, s1
# CHECK: encoding: [0xb7,0x13]
xor s0, s1

# CHECK-INST: rsb s0, s1
# CHECK: encoding: [0xf7,0x13]
rsb s0, s1

# CHECK-INST: adc s0, s1
# CHECK: encoding: [0x37,0x53]
adc s0, s1

# CHECK-INST: sbc s0, s1
# CHECK: encoding: [0x77,0x53]
sbc s0, s1

# CHECK-INST: sra s0, s1
# CHECK: encoding: [0xb7,0x53]
sra s0, s1

# CHECK-INST: or s0, s1
# CHECK: encoding: [0xf7,0x53]
or s0, s1

# CHECK-INST: sll s0, s1
# CHECK: encoding: [0x37,0x93]
sll s0, s1

# CHECK-INST: cmp s0, s1
# CHECK: encoding: [0x77,0x93]
cmp s0, s1

# CHECK-INST: srl s0, s1
# CHECK: encoding: [0xb7,0x93]
srl s0, s1

# CHECK-INST: and s0, s1
# CHECK: encoding: [0xf7,0x93]
and s0, s1

# CHECK-INST: addn s0, s1
# CHECK: encoding: [0x37,0xd3]
addn s0, s1

# CHECK-INST: cmpc s0, s1
# CHECK: encoding: [0x77,0xd3]
cmpc s0, s1

# CHECK-INST: not s0, s1
# CHECK: encoding: [0xb7,0xd3]
not s0, s1

# CHECK-INST: test s0, s1
# CHECK: encoding: [0xf7,0xd3]
test s0, s1

# CHECK-INST: tsti a0, 5
# CHECK: encoding: [0x1f,0x25]
tsti a0, 5

# CHECK-INST: slli a0, 5
# CHECK: encoding: [0x1f,0xa5]
slli a0, 5

# CHECK-INST: srai a0, 5
# CHECK: encoding: [0x9f,0x25]
srai a0, 5

# CHECK-INST: srli a0, 5
# CHECK: encoding: [0x9f,0xa5]
srli a0, 5

# CHECK-INST: addi a0, -6
# CHECK: encoding: [0x5f,0xe2]
addi a0, -6

# CHECK-INST: adci a0, -6
# CHECK: encoding: [0xdf,0xe2]
adci a0, -6

# CHECK-INST: ori a0, -6
# CHECK: encoding: [0x3f,0xe2]
ori a0, -6

# CHECK-INST: cmpi a0, -6
# CHECK: encoding: [0x7f,0xe2]
cmpi a0, -6

# CHECK-INST: lli a0, -6
# CHECK: encoding: [0xbf,0xe2]
lli a0, -6

# CHECK-INST: andi a0, -6
# CHECK: encoding: [0xff,0xe2]
andi a0, -6

# CHECK-INST: csri xs, 5
# CHECK: encoding: [0x57,0x05]
csri xs, 5

# CHECK-INST: csrw ys, a0
# CHECK: encoding: [0x57,0x62]
csrw csr2, a0

# CHECK-INST: csrr a0, ss
# CHECK: encoding: [0x17,0x63]
csrr a0, ss

