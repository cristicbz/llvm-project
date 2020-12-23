//===-- VC16ELFObjectWriter.cpp - VC16 ELF Writer -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/VC16FixupKinds.h"
#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

#include <memory>

using namespace llvm;

namespace {
class VC16ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  VC16ELFObjectWriter(uint8_t OSABI);

  ~VC16ELFObjectWriter() override;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

VC16ELFObjectWriter::VC16ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit*/ false, OSABI, ELF::EM_VC16,
                              /*HasRelocationAddend*/ true) {}

VC16ELFObjectWriter::~VC16ELFObjectWriter() {}

unsigned VC16ELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  switch ((unsigned)Fixup.getKind()) {
  default:
    llvm_unreachable("invalid fixup kind!");
  case FK_Data_4:
    return ELF::R_VC16_32;
  case FK_Data_8:
    return ELF::R_VC16_64;
  case VC16::fixup_vc16_hi11u:
    return ELF::R_VC16_HI11U;
  case VC16::fixup_vc16_hi11s:
    return ELF::R_VC16_HI11S;
  case VC16::fixup_vc16_lo5_mw:
    return ELF::R_VC16_LO5_MW;
  case VC16::fixup_vc16_lo5_mb:
    return ELF::R_VC16_LO5_MB;
  case VC16::fixup_vc16_lo5_ri5:
    return ELF::R_VC16_LO5_RI5;
  case VC16::fixup_vc16_lo5_rri5:
    return ELF::R_VC16_LO5_RRI5;
  case VC16::fixup_vc16_jal:
    return ELF::R_VC16_JAL;
  case VC16::fixup_vc16_branch:
    return ELF::R_VC16_BRANCH;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createVC16ELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<VC16ELFObjectWriter>(OSABI);
}
