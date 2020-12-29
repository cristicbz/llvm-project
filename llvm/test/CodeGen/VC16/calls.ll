; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs -debug < %s \
; RUN:   | FileCheck -check-prefix=VC16I %s

declare i16 @external_function(i16)

define i16 @test_call_external(i16 %a) nounwind {
; VC16I-LABEL: test_call_external:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -2
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    lui a2, %his(external_function)
; VC16I-NEXT:    lea ra, a2, %lo(external_function)
; VC16I-NEXT:    ret
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lea sp, sp, 2
; VC16I-NEXT:    ret
  %1 = call i16 @external_function(i16 %a)
  ret i16 %1
}

define i16 @test_call_external_twice(i16 %a) nounwind {
; VC16I-LABEL: test_call_external_twice:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -8
; VC16I-NEXT:    sw s1, 6(sp)
; VC16I-NEXT:    sw s0, 4(sp)
; VC16I-NEXT:    sw ra, 2(sp)
; VC16I-NEXT:    mv s1, a0
; VC16I-NEXT:    lui a2, %his(external_function)
; VC16I-NEXT:    lea s0, a2, %lo(external_function)
; VC16I-NEXT:    mv ra, s0
; VC16I-NEXT:    ret
; VC16I-NEXT:    sw a0, 0(sp)
; VC16I-NEXT:    mv a0, s1
; VC16I-NEXT:    mv ra, s0
; VC16I-NEXT:    ret
; VC16I-NEXT:    lw a0, 0(sp)
; VC16I-NEXT:    lw ra, 2(sp)
; VC16I-NEXT:    lw s0, 4(sp)
; VC16I-NEXT:    lw s1, 6(sp)
; VC16I-NEXT:    lea sp, sp, 8
; VC16I-NEXT:    ret
  %1 = call i16 @external_function(i16 %a)
  %2 = call i16 @external_function(i16 %a)
  ret i16 %1
}


define i16 @defined_function(i16 %a) nounwind {
; VC16I-LABEL: defined_function:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea a0, a0, 1
; VC16I-NEXT:    ret
  %1 = add i16 %a, 1
  ret i16 %1
}

define i16 @test_call_defined(i16 %a) nounwind {
; VC16I-LABEL: test_call_defined:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -2
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    lui a2, %his(defined_function)
; VC16I-NEXT:    lea ra, a2, %lo(defined_function)
; VC16I-NEXT:    ret
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lea sp, sp, 2
; VC16I-NEXT:    ret
  %1 = call i16 @defined_function(i16 %a) nounwind
  ret i16 %1
}

define i16 @test_call_indirect(i16 (i16)* %a, i16 %b) nounwind {
; VC16I-LABEL: test_call_indirect:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -2
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv ra, a0
; VC16I-NEXT:    mv a0, a1
; VC16I-NEXT:    ret
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lea sp, sp, 2
; VC16I-NEXT:    ret
  %1 = call i16 %a(i16 %b)
  ret i16 %1
}


; Ensure that calls to fastcc functions aren't rejected. Such calls may be
; introduced when compiling with optimisation.

define fastcc i16 @fastcc_function(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: fastcc_function:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    add a0, a1
; VC16I-NEXT:    ret
 %1 = add i16 %a, %b
 ret i16 %1
}

define i16 @test_call_fastcc(i16 %a, i16 %b) nounwind {
; VC16I-LABEL: test_call_fastcc:
; VC16I:       ; %bb.0:
; VC16I-NEXT:    lea sp, sp, -4
; VC16I-NEXT:    sw s1, 2(sp)
; VC16I-NEXT:    sw ra, 0(sp)
; VC16I-NEXT:    mv s1, a0
; VC16I-NEXT:    lui a2, %his(fastcc_function)
; VC16I-NEXT:    lea ra, a2, %lo(fastcc_function)
; VC16I-NEXT:    ret
; VC16I-NEXT:    mv a0, s1
; VC16I-NEXT:    lw ra, 0(sp)
; VC16I-NEXT:    lw s1, 2(sp)
; VC16I-NEXT:    lea sp, sp, 4
; VC16I-NEXT:    ret
  %1 = call fastcc i16 @fastcc_function(i16 %a, i16 %b)
  ret i16 %a
}
