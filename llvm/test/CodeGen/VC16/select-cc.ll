; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=VC16I %s

define i16 @foo(i16 %a, i16 *%b) {
; VC16I-LABEL: foo:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    addi sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, sp
; VC16I-NEXT:    addi s1, 4
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    bz .LBB0_2
; VC16I-NEXT:  ; %bb.1:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_2:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    bnz .LBB0_4
; VC16I-NEXT:  ; %bb.3:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_4:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a2, a0
; VC16I-NEXT:    bn .LBB0_6
; VC16I-NEXT:  ; %bb.5:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_6:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    bnn .LBB0_8
; VC16I-NEXT:  ; %bb.7:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_8:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    bn .LBB0_10
; VC16I-NEXT:  ; %bb.9:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_10:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a2, a0
; VC16I-NEXT:    bnn .LBB0_12
; VC16I-NEXT:  ; %bb.11:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_12:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a2, a0
; VC16I-NEXT:    blt .LBB0_14
; VC16I-NEXT:  ; %bb.13:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_14:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    bge .LBB0_16
; VC16I-NEXT:  ; %bb.15:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_16:
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:    cmp a0, a2
; VC16I-NEXT:    blt .LBB0_18
; VC16I-NEXT:  ; %bb.17:
; VC16I-NEXT:    mv a0, a2
; VC16I-NEXT:  .LBB0_18:
; VC16I-NEXT:    lw a1, 0(a1)
; VC16I-NEXT:    cmp a1, a0
; VC16I-NEXT:    bge .LBB0_20
; VC16I-NEXT:  ; %bb.19:
; VC16I-NEXT:    mv a0, a1
; VC16I-NEXT:  .LBB0_20:
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    addi sp, 4
; VC16I-NEXT:    jalr t0, ra, 0
  %val1 = load volatile i16, i16* %b
  %tst1 = icmp eq i16 %a, %val1
  %val2 = select i1 %tst1, i16 %a, i16 %val1

  %val3 = load volatile i16, i16* %b
  %tst2 = icmp ne i16 %val2, %val3
  %val4 = select i1 %tst2, i16 %val2, i16 %val3

  %val5 = load volatile i16, i16* %b
  %tst3 = icmp ugt i16 %val4, %val5
  %val6 = select i1 %tst3, i16 %val4, i16 %val5

  %val7 = load volatile i16, i16* %b
  %tst4 = icmp uge i16 %val6, %val7
  %val8 = select i1 %tst4, i16 %val6, i16 %val7

  %val9 = load volatile i16, i16* %b
  %tst5 = icmp ult i16 %val8, %val9
  %val10 = select i1 %tst5, i16 %val8, i16 %val9

  %val11 = load volatile i16, i16* %b
  %tst6 = icmp ule i16 %val10, %val11
  %val12 = select i1 %tst6, i16 %val10, i16 %val11

  %val13 = load volatile i16, i16* %b
  %tst7 = icmp sgt i16 %val12, %val13
  %val14 = select i1 %tst7, i16 %val12, i16 %val13

  %val15 = load volatile i16, i16* %b
  %tst8 = icmp sge i16 %val14, %val15
  %val16 = select i1 %tst8, i16 %val14, i16 %val15

  %val17 = load volatile i16, i16* %b
  %tst9 = icmp slt i16 %val16, %val17
  %val18 = select i1 %tst9, i16 %val16, i16 %val17

  %val19 = load volatile i16, i16* %b
  %tst10 = icmp sle i16 %val18, %val19
  %val20 = select i1 %tst10, i16 %val18, i16 %val19

  ret i16 %val20
}
