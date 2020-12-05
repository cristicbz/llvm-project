//===-- VC16MCAsmInfo.h - VC16 Asm Info ------------------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the VC16MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCASMINFO_H
#define LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class VC16MCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit VC16MCAsmInfo(const Triple &TargetTriple);
};

} // namespace llvm

#endif
