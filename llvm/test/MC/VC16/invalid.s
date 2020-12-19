; RUN: not llvm-mc -triple vc16 < %s 2>&1 | FileCheck %s

;; Out of range immediates
ori    x0,  -17 ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]
andi   ra,  16  ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]

;; Invalid mnemonics
subs   x0,  x1  ; CHECK: :[[@LINE]]:1: error: unrecognized instruction mnemonic
nandi  x0,  0   ; CHECK: :[[@LINE]]:1: error: unrecognized instruction mnemonic

;; Invalid register names
addi   foo, 10  ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction
andi   x8,  0x1 ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction
xor    x0,  a4  ; CHECK: :[[@LINE]]:13: error: invalid operand for instruction

;; Invalid operand types
andi   sp,  x0  ; CHECK: :[[@LINE]]:13: error: immediate must be an integer in the range [-16, 15]
sub    1,   x0  ; CHECK: :[[@LINE]]:8: error: invalid operand for instruction

;; Too many operands
add ra, x3, x3  ; CHECK: :[[@LINE]]:13: error: invalid operand for instruction
andi x2, 2, 2  ; CHECK: :[[@LINE]]:13: error: invalid operand for instruction

;; Too few operands
ori x0 ; CHECK: :[[@LINE]]:1: error: too few operands for instruction

