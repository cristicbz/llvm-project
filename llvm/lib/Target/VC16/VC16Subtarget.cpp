//===-- VC16Subtarget.cpp - VC16 Subtarget Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the VC16 specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "VC16Subtarget.h"
#include "VC16.h"
#include "VC16FrameLowering.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "vc16-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "VC16GenSubtargetInfo.inc"

void VC16Subtarget::anchor() {}

VC16Subtarget &VC16Subtarget::initializeSubtargetDependencies(StringRef CPU,
                                                              StringRef FS) {
  // Determine default and user-specified characteristics
  std::string CPUName = std::string(CPU);
  if (CPUName.empty())
    CPUName = "generic-vc16";
  ParseSubtargetFeatures(CPUName, FS);
  return *this;
}

VC16Subtarget::VC16Subtarget(const Triple &TT, StringRef CPU, StringRef FS,
                             const TargetMachine &TM)
    : VC16GenSubtargetInfo(TT, CPU, FS),
      FrameLowering(initializeSubtargetDependencies(CPU, FS)),
      InstrInfo(), RegInfo(getHwMode()), TLInfo(TM, *this) {}
