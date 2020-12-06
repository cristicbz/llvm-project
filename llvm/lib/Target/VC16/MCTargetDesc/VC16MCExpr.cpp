//===-- VC16MCExpr.cpp - VC16 specific MC expression classes --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the VC16 architecture (e.g. ":lo12:", ":gottprel_g1:", ...).
//
//===----------------------------------------------------------------------===//

#include "VC16MCExpr.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Object/ELF.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "vc16mcexpr"

const VC16MCExpr *VC16MCExpr::create(const MCExpr *Expr, VariantKind Kind,
                                       MCContext &Ctx) {
  return new (Ctx) VC16MCExpr(Expr, Kind);
}

void VC16MCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  bool HasVariant = getKind() != VK_VC16_None;
  if (HasVariant)
    OS << '%' << getVariantKindName(getKind()) << '(';
  Expr->print(OS, MAI);
  if (HasVariant)
    OS << ')';
}

bool VC16MCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                            const MCAsmLayout *Layout,
                                            const MCFixup *Fixup) const {
  return getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup);
}

void VC16MCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

VC16MCExpr::VariantKind VC16MCExpr::getVariantKindForName(StringRef name) {
  return StringSwitch<VC16MCExpr::VariantKind>(name)
      .Case("lo", VK_VC16_LO)
      .Case("hiu", VK_VC16_HIU)
      .Case("his", VK_VC16_HIS)
      .Default(VK_VC16_Invalid);
}

StringRef VC16MCExpr::getVariantKindName(VariantKind Kind) {
  switch (Kind) {
  default:
    llvm_unreachable("Invalid ELF symbol kind");
  case VK_VC16_LO:
    return "lo";
  case VK_VC16_HIU:
    return "hiu";
  case VK_VC16_HIS:
    return "his";
  }
}

bool VC16MCExpr::evaluateAsConstant(int64_t &Res) const {
  MCValue Value;

  if (!getSubExpr()->evaluateAsRelocatable(Value, nullptr, nullptr))
    return false;

  if (!Value.isAbsolute())
    return false;

  Res = evaluateAsInt64(Value.getConstant());
  return true;
}

int64_t VC16MCExpr::evaluateAsInt64(int64_t Value) const {
  switch (Kind) {
  default:
    llvm_unreachable("Invalid kind");
  case VK_VC16_LO:
    return SignExtend64<5>(Value);
  case VK_VC16_HIU:
    return (Value >> 5) & 0x7ff;
  case VK_VC16_HIS:
    // Add 1 if bit 4 is 1, to compensate for low 12 bits being negative.
    return ((Value + (1 << 4)) >> 5) & 0x7ff;
  }
}
