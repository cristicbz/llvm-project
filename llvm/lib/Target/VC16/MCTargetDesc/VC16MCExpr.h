//===-- VC16MCExpr.h - VC16 specific MC expression classes ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes VC16-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCEXPR_H
#define LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCEXPR_H

#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class VC16MCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_VC16_None,
    VK_VC16_LO,
    VK_VC16_HIU,
    VK_VC16_HIS,
    VK_VC16_Invalid
  };

private:
  const MCExpr *Expr;
  const VariantKind Kind;

  int64_t evaluateAsInt64(int64_t Value) const;

  explicit VC16MCExpr(const MCExpr *Expr, VariantKind Kind)
      : Expr(Expr), Kind(Kind) {}

public:
  static const VC16MCExpr *create(const MCExpr *Expr, VariantKind Kind,
                                   MCContext &Ctx);

  VariantKind getKind() const { return Kind; }

  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  // There are no TLS VC16MCExprs at the moment.
  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override {}

  bool evaluateAsConstant(int64_t &Res) const;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const VC16MCExpr *) { return true; }

  static VariantKind getVariantKindForName(StringRef name);
  static StringRef getVariantKindName(VariantKind Kind);
};

} // end namespace llvm.

#endif
