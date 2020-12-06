; RUN: not llvm-mc -triple vc16 < %s 2>&1 | FileCheck %s

;; Out of range immediates
ori    r0,  -17 ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]
andi   ra,  16  ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]

;; Invalid mnemonics
subs   r0,  r1  ; CHECK: :[[@LINE]]:1: error: unrecognized instruction mnemonic
nandi  r0,  0   ; CHECK: :[[@LINE]]:1: error: unrecognized instruction mnemonic

;; Invalid register names
addi   foo, 10  ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction
xori   r8,  0x1 ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction
xor    r0,  r32 ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]

;; Invalid operand types
xori   sp,  r0  ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]
sub    1,   r0  ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction

;; Too many operands
add ra, r3, r3  ; CHECK: :[[@LINE]]:13: error: invalid operand for instruction
xori r2, 2, 2  ; CHECK: :[[@LINE]]:13: error: invalid operand for instruction

;; Too few operands
ori r0 ; CHECK: :[[@LINE]]:1: error: too few operands for instruction

