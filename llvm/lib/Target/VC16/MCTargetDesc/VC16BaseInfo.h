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

// VC16II - This namespace holds all of the target specific flags that
// instruction info tracks. All definitions must match VC16InstrFormats.td.
namespace VC16II {
enum {
  InstFormatPseudo = 0,
  InstFormatRI11 = 1,
  InstFormatMW = 2,
  InstFormatMB = 3,
  InstFormatRI10 = 4,
  InstFormatRRI5 = 5,
  InstFormatB = 6,
  InstFormatRR = 7,
  InstFormatRI4 = 8,
  InstFormatRI5 = 9,

  InstFormatMask = 15
};
enum {
  MO_None,
  MO_LO,
  MO_HIU,
  MO_HIS,
};
} // namespace VC16II

} // namespace llvm

#endif
