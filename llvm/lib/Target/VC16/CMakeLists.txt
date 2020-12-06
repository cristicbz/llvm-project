set(LLVM_TARGET_DEFINITIONS VC16.td)

tablegen(LLVM VC16GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM VC16GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM VC16GenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM VC16GenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM VC16GenAsmMatcher.inc -gen-asm-matcher)

add_public_tablegen_target(VC16CommonTableGen)

add_llvm_target(VC16CodeGen
  VC16TargetMachine.cpp
 )

add_subdirectory(AsmParser)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
