//===-- VC16AsmBackend.cpp - VC16 Assembler Backend ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <iostream>

#include "MCTargetDesc/VC16FixupKinds.h"
#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "llvm/ADT/APInt.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class VC16AsmBackend : public MCAsmBackend {
  uint8_t OSABI;

public:
  VC16AsmBackend(uint8_t OSABI) : MCAsmBackend(support::little), OSABI(OSABI) {}
  ~VC16AsmBackend() override {}

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override {
    return false;
  }

  unsigned getNumFixupKinds() const override {
    return VC16::NumTargetFixupKinds;
  }

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
    const static MCFixupKindInfo Infos[VC16::NumTargetFixupKinds] = {
        // This table *must* be in the order that the fixup_* kinds are defined
        // in
        // VC16FixupKinds.h.
        //
        // clang-format off
        // name                    offset bits  flags
        { "fixup_vc16_hi11u",        0,     16,  0 },
        { "fixup_vc16_hi11s",        0,     16,  0 },
        { "fixup_vc16_lo5_mw",       0,     16,  0 },
        { "fixup_vc16_lo5_mb",       0,     16,  0 },
        { "fixup_vc16_lo5_ri5",      0,     16,  0 },
        { "fixup_vc16_lo5_rri5",     0,     16,  0 },
        { "fixup_vc16_jal",          0,     16,  MCFixupKindInfo::FKF_IsPCRel },
        { "fixup_vc16_branch",       8,      8,  MCFixupKindInfo::FKF_IsPCRel },
        // clang-format on
    };

    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);

    assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
           "Invalid kind!");
    return Infos[Kind - FirstTargetFixupKind];
  }

  bool mayNeedRelaxation(const MCInst &Inst,
                         const MCSubtargetInfo &STI) const override {
    return false;
  }

  void relaxInstruction(MCInst &Inst,
                        const MCSubtargetInfo &STI) const override {
    report_fatal_error("VC16AsmBackend::relaxInstruction() unimplemented");
  }

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;
};

bool VC16AsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  if ((Count % 2) != 0)
    return false;

  // The canonical nop on VC16 is 0x0007: lea x0, x0, 0
  for (uint64_t i = 0; i < Count; i += 2)
    OS.write("\x00\x07", 2);

  return true;
}

static uint64_t adjustFixupValue(const MCFixup &Fixup, const uint64_t Value,
                                 MCContext &Ctx) {
  const unsigned Kind = Fixup.getKind();
  switch (Kind) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return Value;

  case VC16::fixup_vc16_hi11u: {
    const uint64_t Adjusted = ((Value & 0xffff) >> 5);
    const uint64_t Msb2 = Adjusted & 0b11000000000;
    const uint64_t Lsb9 = Adjusted & 0b00111111111;
    return (Msb2 << 5) | (Lsb9 << 2);
  }

  case VC16::fixup_vc16_hi11s: {
    const uint64_t Adjusted = ((((Value & 0xffff) + (1 << 4))) >> 5);
    const uint64_t Msb2 = Adjusted & 0b11000000000;
    const uint64_t Lsb9 = Adjusted & 0b00111111111;
    return (Msb2 << 5) | (Lsb9 << 2);
  }

  case VC16::fixup_vc16_lo5_mb: {
    const uint64_t Msb2 = Value & 0b11000;
    const uint64_t Lsb3 = Value & 0b00111;
    return (Msb2 << 11) | (Lsb3 << 3);
  }

  case VC16::fixup_vc16_lo5_mw: {
    if (Value & 0x1)
      Ctx.reportError(Fixup.getLoc(), "MW fixup value must be 2-byte aligned");
    const uint64_t Adjusted = (Value & 0x1f) >> 1;
    const uint64_t Msb2 = Adjusted & 0b11000;
    const uint64_t Lsb3 = Adjusted & 0b00111;
    return (Msb2 << 11) | (Lsb3 << 3);
  }

  case VC16::fixup_vc16_lo5_ri5: {
    const uint64_t Msb2 = Value & 0b11000;
    const uint64_t Lsb3 = Value & 0b00111;
    return (Msb2 << 11) | (Lsb3 << 8);
  }

  case VC16::fixup_vc16_lo5_rri5: {
    const uint64_t Msb2 = Value & 0b11000;
    const uint64_t Lsb3 = Value & 0b00111;
    return (Msb2 << 11) | (Lsb3 << 5);
  }

  case VC16::fixup_vc16_jal: {
    if (!isInt<11>(Value))
      Ctx.reportError(Fixup.getLoc(), "jal fixup value out of range");
    if (Value & 0x1)
      Ctx.reportError(Fixup.getLoc(), "jal fixup value must be 2-byte aligned");
    const uint64_t Adjusted = Value >> 1;
    const uint64_t Msb2 = Adjusted & 0b1100000000;
    const uint64_t Lsb8 = Adjusted & 0b0011111111;
    return (Msb2 << 6) | (Lsb8 << 3);
  }

  case VC16::fixup_vc16_branch:
    if (!isInt<9>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "branch fixup value out of range");
    }
    if (Value & 0x1) {
      Ctx.reportError(Fixup.getLoc(),
                      "branch fixup value must be 2-byte aligned");
    }
    return (Value >> 1) & 0xff;
  };
}

void VC16AsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved,
                                const MCSubtargetInfo *STI) const {
  MCContext &Ctx = Asm.getContext();
  MCFixupKindInfo Info = getFixupKindInfo(Fixup.getKind());
  if (!Value)
    return; // Doesn't change encoding.
  // Apply any target-specific value adjustments.
  Value = adjustFixupValue(Fixup, Value, Ctx);

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();

#ifndef NDEBUG
  unsigned NumBytes = (Info.TargetSize + 7) / 8;
  assert(Offset + NumBytes <= Data.size() && "Invalid fixup offset!");
#endif

  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned i = 0; i != 2; ++i) {
    Data[Offset + i] |= uint8_t((Value >> (i * 8)) & 0xff);
  }
}

std::unique_ptr<MCObjectTargetWriter>
VC16AsmBackend::createObjectTargetWriter() const {
  return createVC16ELFObjectWriter(OSABI);
}

} // end anonymous namespace

MCAsmBackend *llvm::createVC16AsmBackend(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  const Triple &TT = STI.getTargetTriple();
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return new VC16AsmBackend(OSABI);
}
