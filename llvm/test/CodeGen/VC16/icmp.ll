; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck %s -check-prefix=VC16I

; TODO: check the generated instructions for the equivalent of seqz, snez,
; sltz, sgtz map to something simple

define i16 @icmp_eq(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_eq:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a2, a1
; VC16I-NEXT:    bz .LBB0_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB0_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp eq i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_ugt(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_ugt:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a1, a2
; VC16I-NEXT:    bn .LBB1_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB1_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp ugt i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_uge(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_uge:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a2, a1
; VC16I-NEXT:    bnn .LBB2_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB2_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp uge i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_ult(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_ult:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a2, a1
; VC16I-NEXT:    bn .LBB3_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB3_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp ult i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_ule(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_ule:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a1, a2
; VC16I-NEXT:    bnn .LBB4_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB4_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp ule i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}


define i16 @icmp_sgt(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_sgt:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a1, a2
; VC16I-NEXT:    blt .LBB5_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB5_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp sgt i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_sge(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_sge:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a2, a1
; VC16I-NEXT:    bge .LBB6_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB6_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp sge i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}

define i16 @icmp_slt(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_slt:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a2, a1
; VC16I-NEXT:    blt .LBB7_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB7_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp slt i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}


define i16 @icmp_sle(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: icmp_sle:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s0, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s0, sp
; VC16I-NEXT:    addi s0, 4
; VC16I-NEXT:    mv a2, a0
; VC16I-NEXT:    lli a0, 1
; VC16I-NEXT:    cmp a1, a2
; VC16I-NEXT:    bge .LBB8_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    lli a0, 0
; VC16I-NEXT:  .LBB8_2:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s0, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr ra, 0
  %1 = icmp sle i16 %a, %b
  %2 = zext i1 %1 to i16
  ret i16 %2
}
