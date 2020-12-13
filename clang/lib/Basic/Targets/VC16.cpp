//===--- VC16.cpp - Implement VC16 target feature support -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements VC16 TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "VC16.h"
#include "clang/Basic/MacroBuilder.h"

using namespace clang;
using namespace clang::targets;

const char *const VC16TargetInfo::GCCRegNames[] = {
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
};

ArrayRef<const char *> VC16TargetInfo::getGCCRegNames() const {
  return llvm::makeArrayRef(GCCRegNames);
}

void VC16TargetInfo::getTargetDefines(const LangOptions &Opts,
                                      MacroBuilder &Builder) const {
  Builder.defineMacro("VC16");
  Builder.defineMacro("__VC16__");
  // FIXME: defines for different 'flavours' of MCU
}
