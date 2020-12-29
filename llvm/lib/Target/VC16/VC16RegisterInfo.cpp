//===-- VC16RegisterInfo.cpp - VC16 Register Information ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the VC16 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "VC16RegisterInfo.h"
#include "VC16.h"
#include "VC16Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_REGINFO_TARGET_DESC
#include "VC16GenRegisterInfo.inc"

using namespace llvm;

VC16RegisterInfo::VC16RegisterInfo(unsigned HwMode)
    : VC16GenRegisterInfo(VC16::X1, /*DwarfFlavour*/ 0, /*EHFlavor*/ 0,
                          /*PC*/ 0, HwMode) {}

const MCPhysReg *
VC16RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

BitVector VC16RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  const VC16FrameLowering *TFI = getFrameLowering(MF);
  BitVector Reserved(getNumRegs());

  // Use markSuperRegs to ensure any register aliases are also reserved
  markSuperRegs(Reserved, VC16::X0);    // sp
  markSuperRegs(Reserved, VC16::CS);    // code segment
  markSuperRegs(Reserved, VC16::SS);    // stack segment
  markSuperRegs(Reserved, VC16::FLAGS); // flags
  if (TFI->hasFP(MF))
    markSuperRegs(Reserved, VC16::X2); // fp
  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

const uint32_t *VC16RegisterInfo::getNoPreservedMask() const {
  return CSR_NoRegs_RegMask;
}

void VC16RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                           int SPAdj, unsigned FIOperandNum,
                                           RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected non-zero SPAdj value");

  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const VC16InstrInfo *TII = MF.getSubtarget<VC16Subtarget>().getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  Register FrameReg;
  int Offset =
      getFrameLowering(MF)->getFrameIndexReference(MF, FrameIndex, FrameReg) +
      MI.getOperand(FIOperandNum + 1).getImm();

  if (!isUInt<16>(Offset)) {
    report_fatal_error("Frame offsets outside of the unsigned 16-bit range.");
  }

  MachineBasicBlock &MBB = *MI.getParent();
  unsigned OpCode = MI.getOpcode();

  int64_t BoundedOffset;
  switch (OpCode) {
  case VC16::LEA:
    BoundedOffset = !isInt<6>(Offset)
                        ? SignExtend64<5>(Offset)
                        : Offset < -16 ? -16 : Offset > 15 ? 15 : Offset;
    break;
  case VC16::LB:
  case VC16::SB:
    BoundedOffset =
        (Offset > (31 + 15)) ? Offset & 31 : Offset > 31 ? 31 : Offset;
    break;
  case VC16::LW:
  case VC16::SW:
    BoundedOffset =
        (Offset > (62 + 14)) ? Offset & 63 : Offset > 62 ? 62 : Offset;
    break;
  default:
    MI.dump();
    llvm_unreachable("Unknown operation for eliminateFrameIndex in operation");
  }

  if (Offset == BoundedOffset) {
    MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }

  // The offset won't fit in an immediate, so use a scratch register
  // instead Modify Offset and FrameReg appropriately
  assert(isInt<16>(Offset) && "Int16 expected");
  Register ScratchReg = MRI.createVirtualRegister(&VC16::GPRRegClass);
  Offset -= BoundedOffset;
  if (isInt<5>(Offset)) {
    BuildMI(MBB, II, DL, TII->get(VC16::LEA), ScratchReg)
        .addReg(FrameReg)
        .addImm(Offset);
  } else {
    BuildMI(MBB, II, DL, TII->get(VC16::LUI), ScratchReg).addImm(Offset >> 5);
    BuildMI(MBB, II, DL, TII->get(VC16::ADDN), ScratchReg)
        .addReg(ScratchReg, RegState::Kill)
        .addReg(FrameReg);
  }
  MI.getOperand(FIOperandNum)
      .ChangeToRegister(ScratchReg, false, false, /* isKill = */ true);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(BoundedOffset);
}

Register VC16RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  return TFI->hasFP(MF) ? VC16::X2 : VC16::X0;
}

const uint32_t *
VC16RegisterInfo::getCallPreservedMask(const MachineFunction & /*MF*/,
                                       CallingConv::ID /*CC*/) const {
  return CSR_RegMask;
}
