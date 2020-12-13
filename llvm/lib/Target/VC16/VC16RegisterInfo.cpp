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
    : VC16GenRegisterInfo(VC16::R0, /*DwarfFlavour*/ 0, /*EHFlavor*/ 0,
                          /*PC*/ 0, HwMode) {}

const MCPhysReg *
VC16RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

BitVector VC16RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  // Use markSuperRegs to ensure any register aliases are also reserved
  markSuperRegs(Reserved, VC16::R4); // fp
  markSuperRegs(Reserved, VC16::R6); // ra
  markSuperRegs(Reserved, VC16::R7); // sp
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
  DebugLoc DL = MI.getDebugLoc();

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  Register FrameReg;
  int Offset =
      getFrameLowering(MF)->getFrameIndexReference(MF, FrameIndex, FrameReg) +
      MI.getOperand(FIOperandNum + 1).getImm();

  assert(MF.getSubtarget().getFrameLowering()->hasFP(MF) &&
         "eliminateFrameIndex currently requires hasFP");

  // Offsets must be directly encoded in a 12-bit immediate field
  if (!isUInt<5>(Offset)) {
    report_fatal_error(
        "Frame offsets outside of the unsigned 5-bit range not supported");
  }

  MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
}

Register VC16RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return VC16::R4;
}

const uint32_t *
VC16RegisterInfo::getCallPreservedMask(const MachineFunction & /*MF*/,
                                       CallingConv::ID /*CC*/) const {
  return CSR_RegMask;
}
