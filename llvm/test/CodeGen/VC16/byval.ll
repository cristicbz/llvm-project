; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=vc16 -verify-machineinstrs -debug < %s \
; RUN:   | FileCheck -check-prefix=VC16I %s

%struct.Foo = type { i16, i16, i16, i16, i8 }
@foo = global %struct.Foo { i16 1, i16 2, i16 3, i16 4, i8 5 }, align 2

define i16 @callee(%struct.Foo* byval %f) nounwind {
; VC16I-LABEL: callee:
; VC16I:       ; %bb.0: ; %entry
; VC16I-NEXT:    lw a0, 0(a0)
; VC16I-NEXT:    jalr ra, 0
entry:
  %0 = getelementptr inbounds %struct.Foo, %struct.Foo* %f, i32 0, i32 0
  %1 = load i16, i16* %0, align 2
  ret i16 %1
}


define void @caller() nounwind {
; VC16I-LABEL: caller:
; VC16I:       ; %bb.0: ; %entry
; VC16I-NEXT:    lea sp, sp, -12
; VC16I-NEXT:    sw ra, 10(sp)
; VC16I-NEXT:    lui a2, %his(foo+8)
; VC16I-NEXT:    lea a2, a2, %lo(foo+8)
; VC16I-NEXT:    lw a2, 0(a2)
; VC16I-NEXT:    sw a2, 8(sp)
; VC16I-NEXT:    lui a2, %his(foo+6)
; VC16I-NEXT:    lea a2, a2, %lo(foo+6)
; VC16I-NEXT:    lw a2, 0(a2)
; VC16I-NEXT:    sw a2, 6(sp)
; VC16I-NEXT:    lui a2, %his(foo+4)
; VC16I-NEXT:    lea a2, a2, %lo(foo+4)
; VC16I-NEXT:    lw a2, 0(a2)
; VC16I-NEXT:    sw a2, 4(sp)
; VC16I-NEXT:    lui a2, %his(foo+2)
; VC16I-NEXT:    lea a2, a2, %lo(foo+2)
; VC16I-NEXT:    lw a2, 0(a2)
; VC16I-NEXT:    sw a2, 2(sp)
; VC16I-NEXT:    lui a2, %his(foo)
; VC16I-NEXT:    lea a2, a2, %lo(foo)
; VC16I-NEXT:    lw a2, 0(a2)
; VC16I-NEXT:    sw a2, 0(sp)
; VC16I-NEXT:    lui a2, %his(callee)
; VC16I-NEXT:    lea ra, a2, %lo(callee)
; VC16I-NEXT:    lea a0, sp, 0
; VC16I-NEXT:    jalr ra, 0
; VC16I-NEXT:    lw ra, 10(sp)
; VC16I-NEXT:    lea sp, sp, 12
; VC16I-NEXT:    jalr ra, 0
entry:
  %call = call i16 @callee(%struct.Foo* byval @foo)
  ret void
}
