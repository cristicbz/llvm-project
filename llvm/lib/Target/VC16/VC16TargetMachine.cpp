//===-- VC16TargetMachine.cpp - Define TargetMachine for VC16 -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about VC16 target spec.
//
//===----------------------------------------------------------------------===//

#include "VC16TargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"

#include <memory>

using namespace llvm;

extern "C" void LLVMInitializeVC16Target() {
  RegisterTargetMachine<VC16TargetMachine> X(getTheVC16Target());
}

static std::string computeDataLayout(const Triple &TT) {
  return "e-m:e-p:16:16-i32:16-i64:16-f32:16-f64:16-n16-S16";
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           Optional<Reloc::Model> RM) {
  if (!RM.hasValue())
    return Reloc::Static;
  return *RM;
}

VC16TargetMachine::VC16TargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     Optional<Reloc::Model> RM,
                                     Optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        getEffectiveRelocModel(TT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

namespace {
class VC16PassConfig : public TargetPassConfig {
public:
  VC16PassConfig(VC16TargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  VC16TargetMachine &getVC16TargetMachine() const {
    return getTM<VC16TargetMachine>();
  }

  bool addInstSelector() override;
};
} // namespace

TargetPassConfig *VC16TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new VC16PassConfig(*this, PM);
}

bool VC16PassConfig::addInstSelector() {
  addPass(createVC16ISelDag(getVC16TargetMachine()));

  return false;
}
