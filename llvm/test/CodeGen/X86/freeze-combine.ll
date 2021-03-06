; NOTE: Assertions have been autogenerated by utils/update_mir_test_checks.py
; RUN: llc -stop-after=finalize-isel -mtriple=x86_64-unknown < %s 2>&1 | FileCheck %s
define i32 @const() {
  ; CHECK-LABEL: name: const
  ; CHECK: bb.0 (%ir-block.0):
  ; CHECK:   [[MOV32ri:%[0-9]+]]:gr32 = MOV32ri 1
  ; CHECK:   $eax = COPY [[MOV32ri]]
  ; CHECK:   RET 0, $eax
  %y = freeze i32 1
  ret i32 %y
}

define i32 @fold(i32 %x) {
  ; CHECK-LABEL: name: fold
  ; CHECK: bb.0 (%ir-block.0):
  ; CHECK:   liveins: $edi
  ; CHECK:   [[COPY:%[0-9]+]]:gr32 = COPY $edi
  ; CHECK:   [[COPY1:%[0-9]+]]:gr32 = COPY [[COPY]]
  ; CHECK:   $eax = COPY [[COPY1]]
  ; CHECK:   RET 0, $eax
  %y = freeze i32 %x
  %z = freeze i32 %y
  ret i32 %z
}
