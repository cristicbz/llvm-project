//===-- VC16MCTargetDesc.cpp - VC16 Target Descriptions -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// This file provides VC16-specific target descriptions.
///
//===----------------------------------------------------------------------===//

#include "VC16MCTargetDesc.h"
#include "VC16MCAsmInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "VC16GenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "VC16GenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "VC16GenSubtargetInfo.inc"


using namespace llvm;

static MCInstrInfo *createVC16MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitVC16MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createVC16MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitVC16MCRegisterInfo(X, VC16::R1);
  return X;
}

static MCAsmInfo *createVC16MCAsmInfo(const MCRegisterInfo &MRI,
                                      const Triple &TT,
                                      const MCTargetOptions &Options) {
  return new VC16MCAsmInfo(TT);
}

static MCSubtargetInfo *createVC16MCSubtargetInfo(const Triple &TT,
                                                   StringRef CPU, StringRef FS) {
  std::string CPUName = std::string(CPU);
  if (CPUName.empty())
    CPUName = "generic-vc16";
  return createVC16MCSubtargetInfoImpl(TT, CPUName, FS);
}

extern "C" void LLVMInitializeVC16TargetMC() {
  Target &T = getTheVC16Target();
  TargetRegistry::RegisterMCAsmInfo(T, createVC16MCAsmInfo);
  TargetRegistry::RegisterMCInstrInfo(T, createVC16MCInstrInfo);
  TargetRegistry::RegisterMCRegInfo(T, createVC16MCRegisterInfo);
  TargetRegistry::RegisterMCAsmBackend(T, createVC16AsmBackend);
  TargetRegistry::RegisterMCCodeEmitter(T, createVC16MCCodeEmitter);
  TargetRegistry::RegisterMCSubtargetInfo(T, createVC16MCSubtargetInfo);
}
