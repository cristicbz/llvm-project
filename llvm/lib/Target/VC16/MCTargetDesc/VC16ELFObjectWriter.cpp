//===-- VC16ELFObjectWriter.cpp - VC16 ELF Writer -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

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
}

VC16ELFObjectWriter::VC16ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(/*Is64Bit*/ false, OSABI, ELF::EM_VC16,
                              /*HasRelocationAddend*/ true) {}

VC16ELFObjectWriter::~VC16ELFObjectWriter() {}

unsigned VC16ELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  report_fatal_error("invalid fixup kind!");
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createVC16ELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<VC16ELFObjectWriter>(OSABI);
}
