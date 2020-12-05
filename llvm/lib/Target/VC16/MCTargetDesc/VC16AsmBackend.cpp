//===-- VC16AsmBackend.cpp - VC16 Assembler Backend ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
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
  VC16AsmBackend(uint8_t OSABI)
      : MCAsmBackend(support::little), OSABI(OSABI) {}
  ~VC16AsmBackend() override {}

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved, const MCSubtargetInfo *STI) const override;

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override {
    return false;
  }

  unsigned getNumFixupKinds() const override { return 1; }

  bool mayNeedRelaxation(const MCInst &Inst, const MCSubtargetInfo &STI) const override { return false; }

  void relaxInstruction(MCInst &Inst, const MCSubtargetInfo &STI) const override {
    report_fatal_error("VC16AsmBackend::relaxInstruction() unimplemented");
  }

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;
};

bool VC16AsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  // Once support for the compressed instruction set is added, we will be able
  // to conditionally support 16-bit NOPs
  if ((Count % 2) != 0)
    return false;

  // The canonical nop on VC16 is 0xffff: andi x7, -1
  for (uint64_t i = 0; i < Count; i += 2)
    OS.write("\xff\xff", 2);

  return true;
}

void VC16AsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target, MutableArrayRef<char> Data,
                                uint64_t Value, bool IsResolved, const MCSubtargetInfo *STI) const {
  return;
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
