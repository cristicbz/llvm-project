//===--- VC16.h - Declare VC16 target feature support -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares VC16 TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_VC16_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_VC16_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY VC16TargetInfo : public TargetInfo {
  static const char *const GCCRegNames[];

public:
  VC16TargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple) {
    TLSSupported = false;
    IntWidth = 16;
    IntAlign = 16;
    LongWidth = 32;
    LongLongWidth = 64;
    LongAlign = LongLongAlign = 16;
    FloatWidth = 32;
    FloatAlign = 16;
    DoubleWidth = LongDoubleWidth = 64;
    DoubleAlign = LongDoubleAlign = 16;
    PointerWidth = 16;
    PointerAlign = 16;
    SuitableAlign = 16;
    SizeType = UnsignedInt;
    IntMaxType = SignedLongLong;
    IntPtrType = SignedInt;
    PtrDiffType = SignedInt;
    SigAtomicType = SignedLong;
    resetDataLayout("e-m:e-p:16:16-i32:16-i64:16-f32:16-f64:16-n16-S16");
  }
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  ArrayRef<Builtin::Info> getTargetBuiltins() const override {
    // FIXME: Implement.
    return None;
  }

  bool allowsLargerPreferedTypeAlignment() const override { return false; }

  bool hasFeature(StringRef Feature) const override {
    return Feature == "vc16";
  }

  ArrayRef<const char *> getGCCRegNames() const override;

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    static const TargetInfo::GCCRegAlias GCCRegAliases[] = {
        {{"sp"}, "x0"}, {{"ra"}, "x1"}, {{"s0"}, "x2"}, {{"s1"}, "x4"},
        {{"a0"}, "x4"}, {{"a1"}, "x5"}, {{"a2"}, "x6"}, {{"t0"}, "x7"},
    };
    return llvm::makeArrayRef(GCCRegAliases);
  }

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &info) const override {
    // No target constraints for now.
    return false;
  }

  const char *getClobbers() const override {
    // FIXME: Is this really right?
    return "";
  }

  BuiltinVaListKind getBuiltinVaListKind() const override {
    // FIXME: implement
    return TargetInfo::CharPtrBuiltinVaList;
  }
};

} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_VC16_H
