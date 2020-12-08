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
    : VC16GenRegisterInfo(VC16::R0, /*DwarfFlavour*/0, /*EHFlavor*/0,
                           /*PC*/0, HwMode) {}

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

void VC16RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                            int SPAdj, unsigned FIOperandNum,
                                            RegScavenger *RS) const {
  report_fatal_error("Subroutines not supported yet");
}

Register VC16RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return VC16::R4;
}
