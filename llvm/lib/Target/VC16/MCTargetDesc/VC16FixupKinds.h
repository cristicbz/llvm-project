//===-- VC16FixupKinds.h - VC16 Specific Fixup Entries ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16FIXUPKINDS_H
#define LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16FIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#undef VC16

namespace llvm {
namespace VC16 {
enum Fixups {
  // fixup_vc16_hi11u - 11-bit fixup corresponding to hiu(foo) for lui followed
  // by sign-extended lo5 (addi etc.)
  fixup_vc16_hi11u = FirstTargetFixupKind,

  // fixup_vc16_hi11s - 11-bit fixup corresponding to his(foo) for lui followed
  // by zero-extended lo5 (lw, lb, jalr etc.)
  fixup_vc16_hi11s,

  // fixup_vc16_lo5_mw - 5-bit fixup corresponding to lo(foo) for word-oriented
  // memory instructions
  fixup_vc16_lo5_mw,

  // fixup_vc16_lo5_mb - 5-bit fixup corresponding to lo(foo) for byte-oriented
  // memory instructions
  fixup_vc16_lo5_mb,

  // fixup_vc16_lo5_ri5 - 5-bit fixup corresponding to lo(foo) for instructions
  // like addi and jalr
  fixup_vc16_lo5_ri5,

  // fixup_vc16_lo5_rri5 - 5-bit fixup corresponding to lo(foo) for lea
  fixup_vc16_lo5_rri5,

  // fixup_vc16_jal - 10-bit fixup corresponding to for jal
  fixup_vc16_jal,

  // fixup_vc16_branch - 9-bit fixup for symbol references in the branch
  // instructions
  fixup_vc16_branch,

  // fixup_vc16_invalid - used as a sentinel and a marker, must be last fixup
  fixup_vc16_invalid,
  NumTargetFixupKinds = fixup_vc16_invalid - FirstTargetFixupKind
};
} // end namespace VC16
} // end namespace llvm

#endif
