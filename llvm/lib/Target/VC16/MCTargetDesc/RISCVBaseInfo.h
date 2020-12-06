//===-- VC16BaseInfo.h - Top level definitions for VC16 MC ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone enum definitions for the VC16 target
// useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16BASEINFO_H
#define LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16BASEINFO_H

#include "VC16MCTargetDesc.h"

namespace llvm {

// VC16 - This namespace holds all of the target specific flags that
// instruction info tracks. All definitions must match VC16InstrFormats.td.
namespace VC16 {
enum {
  MO_None,
  MO_LO,
  MO_HI,
};
} // namespace VC16

} // namespace llvm

#endif
