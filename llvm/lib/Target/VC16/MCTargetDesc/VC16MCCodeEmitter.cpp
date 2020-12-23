//===-- VC16MCCodeEmitter.cpp - Convert VC16 code to machine code -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the VC16MCCodeEmitter class.
//
//===----------------------------------------------------------------------===//
//
#include "MCTargetDesc/VC16BaseInfo.h"
#include "MCTargetDesc/VC16FixupKinds.h"
#include "MCTargetDesc/VC16MCExpr.h"
#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");
STATISTIC(MCNumFixups, "Number of MC fixups created");

namespace {
class VC16MCCodeEmitter : public MCCodeEmitter {
  VC16MCCodeEmitter(const VC16MCCodeEmitter &) = delete;
  void operator=(const VC16MCCodeEmitter &) = delete;
  MCContext &Ctx;
  MCInstrInfo const &MCII;

public:
  VC16MCCodeEmitter(MCContext &ctx, MCInstrInfo const &MCII)
      : Ctx(ctx), MCII(MCII) {}

  ~VC16MCCodeEmitter() override {}

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  /// TableGen'erated function for getting the binary encoding for an
  /// instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Return binary encoding of operand. If the machine operand requires
  /// relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  int64_t getImmOpValueAsr1(const MCInst &MI, unsigned OpNo,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const;

  uint64_t getImmOpValueLsr1(const MCInst &MI, unsigned OpNo,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  uint64_t getImmOpValue(const MCInst &MI, unsigned OpNo,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const;
};
} // end anonymous namespace

MCCodeEmitter *llvm::createVC16MCCodeEmitter(const MCInstrInfo &MCII,
                                             const MCRegisterInfo &MRI,
                                             MCContext &Ctx) {
  return new VC16MCCodeEmitter(Ctx, MCII);
}

void VC16MCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const {
  uint16_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
  support::endian::write(OS, Bits, support::little);
  ++MCNumEmitted; // Keep track of the # of mi's emitted.
}

unsigned
VC16MCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                                     SmallVectorImpl<MCFixup> &Fixups,
                                     const MCSubtargetInfo &STI) const {

  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  llvm_unreachable("Unhandled expression!");
  return 0;
}

int64_t VC16MCCodeEmitter::getImmOpValueAsr1(const MCInst &MI, unsigned OpNo,
                                             SmallVectorImpl<MCFixup> &Fixups,
                                             const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    int64_t Res = MO.getImm();
    assert((Res & 1) == 0 && "LSB is non-zero");
    return Res >> 1;
  }

  return getImmOpValue(MI, OpNo, Fixups, STI);
}

uint64_t
VC16MCCodeEmitter::getImmOpValueLsr1(const MCInst &MI, unsigned OpNo,
                                     SmallVectorImpl<MCFixup> &Fixups,
                                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    uint64_t Res = MO.getImm();
    assert((Res & 1) == 0 && "LSB is non-zero");
    return Res >> 1;
  }

  return getImmOpValue(MI, OpNo, Fixups, STI);
}

uint64_t VC16MCCodeEmitter::getImmOpValue(const MCInst &MI, unsigned OpNo,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const {
  MCInstrDesc const &Desc = MCII.get(MI.getOpcode());
  unsigned MIFrm = Desc.TSFlags & VC16II::InstFormatMask;
  const MCOperand &MO = MI.getOperand(OpNo);

  // If the destination is an immediate, there is nothing to do
  if (MO.isImm())
    return MO.getImm();

  assert(MO.isExpr() && "getImmOpValue expects only expressions or immediates");
  const MCExpr *Expr = MO.getExpr();
  MCExpr::ExprKind Kind = Expr->getKind();
  VC16::Fixups FixupKind = VC16::fixup_vc16_invalid;
  if (Kind == MCExpr::Target) {
    const VC16MCExpr *RVExpr = cast<VC16MCExpr>(Expr);

    switch (RVExpr->getKind()) {
    case VC16MCExpr::VK_VC16_None:
    case VC16MCExpr::VK_VC16_Invalid:
      llvm_unreachable("Unhandled fixup kind!");
    case VC16MCExpr::VK_VC16_LO:
      if (MIFrm == VC16II::InstFormatMW)
        FixupKind = VC16::fixup_vc16_lo5_mw;
      else if (MIFrm == VC16II::InstFormatMB)
        FixupKind = VC16::fixup_vc16_lo5_mb;
      else if (MIFrm == VC16II::InstFormatRI5)
        FixupKind = VC16::fixup_vc16_lo5_ri5;
      else if (MIFrm == VC16II::InstFormatRRI5)
        FixupKind = VC16::fixup_vc16_lo5_rri5;
      else
        llvm_unreachable("VK_VC16_LO used with unexpected instruction format");
      break;
    case VC16MCExpr::VK_VC16_HIS:
      FixupKind = VC16::fixup_vc16_hi11s;
      break;
    case VC16MCExpr::VK_VC16_HIU:
      FixupKind = VC16::fixup_vc16_hi11u;
      break;
    }
  } else if (Kind == MCExpr::SymbolRef &&
             cast<MCSymbolRefExpr>(Expr)->getKind() ==
                 MCSymbolRefExpr::VK_None) {
    if (Desc.getOpcode() == VC16::JAL || Desc.getOpcode() == VC16::J) {
      FixupKind = VC16::fixup_vc16_jal;
    } else if (MIFrm == VC16II::InstFormatB) {
      FixupKind = VC16::fixup_vc16_branch;
    }
  }

  assert(FixupKind != VC16::fixup_vc16_invalid && "Unhandled expression!");

  Fixups.push_back(
      MCFixup::create(0, Expr, MCFixupKind(FixupKind), MI.getLoc()));
  ++MCNumFixups;

  return 0;
}

#include "VC16GenMCCodeEmitter.inc"
