; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs -debug < %s \
; RUN:   | FileCheck -check-prefix=VC16I %s

define void @foo(i16 %a, i16 *%b, i1 %c) {
; VC16I-LABEL: foo:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    bz .LBB0_12
; VC16I-NEXT:  ; %bb.1: ; %test2
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    bnz .LBB0_12
; VC16I-NEXT:  ; %bb.2: ; %test3
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    blt .LBB0_12
; VC16I-NEXT:  ; %bb.3: ; %test4
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    bge .LBB0_12
; VC16I-NEXT:  ; %bb.4: ; %test5
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    bn .LBB0_12
; VC16I-NEXT:  ; %bb.5: ; %test6
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp t0, a0
; VC16I-NEXT:    bnn .LBB0_12
; VC16I-NEXT:  ; %bb.6: ; %test7
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp a0, t0
; VC16I-NEXT:    blt .LBB0_12
; VC16I-NEXT:  ; %bb.7: ; %test8
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp a0, t0
; VC16I-NEXT:    bge .LBB0_12
; VC16I-NEXT:  ; %bb.8: ; %test9
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp a0, t0
; VC16I-NEXT:    bn .LBB0_12
; VC16I-NEXT:  ; %bb.9: ; %test10
; VC16I-NEXT:    lw t0, 0(a1)
; VC16I-NEXT:    cmp a0, t0
; VC16I-NEXT:    bnn .LBB0_12
; VC16I-NEXT:  ; %bb.10: ; %test11
; VC16I-NEXT:    lw a0, 0(a1)
; VC16I-NEXT:    tsti a2, 1
; VC16I-NEXT:    bnz .LBB0_12
; VC16I-NEXT:  ; %bb.11: ; %test12
; VC16I-NEXT:    lw a2, 0(a1)
; VC16I-NEXT:  .LBB0_12: ; %end
; VC16I-NEXT:    ret

  %val1 = load volatile i16, i16* %b
  %tst1 = icmp eq i16 %val1, %a
  br i1 %tst1, label %end, label %test2

test2:
  %val2 = load volatile i16, i16* %b
  %tst2 = icmp ne i16 %val2, %a
  br i1 %tst2, label %end, label %test3

test3:
  %val3 = load volatile i16, i16* %b
  %tst3 = icmp slt i16 %val3, %a
  br i1 %tst3, label %end, label %test4

test4:
  %val4 = load volatile i16, i16* %b
  %tst4 = icmp sge i16 %val4, %a
  br i1 %tst4, label %end, label %test5

test5:
  %val5 = load volatile i16, i16* %b
  %tst5 = icmp ult i16 %val5, %a
  br i1 %tst5, label %end, label %test6

test6:
  %val6 = load volatile i16, i16* %b
  %tst6 = icmp uge i16 %val6, %a
  br i1 %tst6, label %end, label %test7

; Check for condition codes that don't have a matching instruction

test7:
  %val7 = load volatile i16, i16* %b
  %tst7 = icmp sgt i16 %val7, %a
  br i1 %tst7, label %end, label %test8

test8:
  %val8 = load volatile i16, i16* %b
  %tst8 = icmp sle i16 %val8, %a
  br i1 %tst8, label %end, label %test9

test9:
  %val9 = load volatile i16, i16* %b
  %tst9 = icmp ugt i16 %val9, %a
  br i1 %tst9, label %end, label %test10

test10:
  %val10 = load volatile i16, i16* %b
  %tst10 = icmp ule i16 %val10, %a
  br i1 %tst10, label %end, label %test11

; Check the case of a branch where the condition was generated in another
; function

test11:
  %val11 = load volatile i16, i16* %b
  br i1 %c, label %end, label %test12

test12:
  %val12 = load volatile i16, i16* %b
  br label %end

end:
  ret void
}
