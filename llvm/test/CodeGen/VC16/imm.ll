; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

; Materializing constants

define i16 @zero() nounwind {
; VC16I-LABEL: zero:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 0
}

define i16 @pos_small() nounwind {
; VC16I-LABEL: pos_small:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lli a0, 15
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 15
}

define i16 @neg_small() nounwind {
; VC16I-LABEL: neg_small:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lli a0, -16
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 -16
}

define i16 @pos_one_too_big() nounwind {
; VC16I-LABEL: pos_one_too_big:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 1
; VC16I-NEXT:    addi a0, -16
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 16
}

define i16 @neg_one_too_big() nounwind {
; VC16I-LABEL: neg_one_too_big:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 2047
; VC16I-NEXT:    addi a0, 15
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 -17
}

define i16 @pos_i16() nounwind {
; VC16I-LABEL: pos_i16:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 1021
; VC16I-NEXT:    addi a0, -14
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 32658
}

define i16 @neg_i16() nounwind {
; VC16I-LABEL: neg_i16:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 1025
; VC16I-NEXT:    addi a0, -10
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 -32746
}

define i16 @pos_multiple_of_32() nounwind {
; VC16I-LABEL: pos_multiple_of_32:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 125
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 4000
}

define i16 @neg_multiple_of_32() nounwind {
; VC16I-LABEL: neg_multiple_of_32:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 1025
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 -32736
}

define i16 @unsigned_multiple_of_32() nounwind {
; VC16I-LABEL: unsigned_multiple_of_32:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    lui a0, 1025
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  ret i16 32800
}
