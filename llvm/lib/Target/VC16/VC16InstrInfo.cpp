//===-- VC16InstrInfo.cpp - VC16 Instruction Information ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the VC16 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "VC16InstrInfo.h"
#include "VC16.h"
#include "VC16Subtarget.h"
#include "VC16TargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "VC16GenInstrInfo.inc"

using namespace llvm;

VC16InstrInfo::VC16InstrInfo()
    : VC16GenInstrInfo(VC16::ADJCALLSTACKDOWN, VC16::ADJCALLSTACKUP) {}

void VC16InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MBBI,
                                const DebugLoc &DL, MCRegister DstReg,
                                MCRegister SrcReg, bool KillSrc) const {
  assert(VC16::GPRRegClass.contains(DstReg, SrcReg) &&
         "Impossible reg-to-reg copy");

  BuildMI(MBB, MBBI, DL, get(VC16::MV), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
}

void VC16InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator I,
                                        Register SrcReg, bool IsKill, int FI,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  if (VC16::GPRRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(VC16::SW))
        .addReg(SrcReg, getKillRegState(IsKill))
        .addFrameIndex(FI)
        .addImm(0);
  else
    llvm_unreachable("Can't store this register to stack slot");
}

void VC16InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator I,
                                         Register DstReg, int FI,
                                         const TargetRegisterClass *RC,
                                         const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  if (VC16::GPRRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(VC16::LW), DstReg).addFrameIndex(FI).addImm(0);
  else
    llvm_unreachable("Can't load this register from stack slot");
}

void VC16InstrInfo::movImm16(MachineBasicBlock &MBB,
                             MachineBasicBlock::iterator MBBI,
                             const DebugLoc &DL, Register DstReg, uint64_t Val,
                             MachineInstr::MIFlag Flag) const {
  assert(isInt<16>(Val) && "Can only materialize 16-bit constants");

  uint64_t Hi11 = ((Val + (1 << 4)) >> 5) & 0x7ff;
  uint64_t Lo5 = SignExtend64<5>(Val);
  if (Hi11 != 0) {
    BuildMI(MBB, MBBI, DL, get(VC16::LUI), DstReg).addImm(Hi11).setMIFlag(Flag);
    if (Lo5 != 0) {
      BuildMI(MBB, MBBI, DL, get(VC16::ADDI), DstReg)
          .addReg(DstReg, RegState::Kill)
          .addImm(Lo5)
          .setMIFlag(Flag);
    }
  } else {
    BuildMI(MBB, MBBI, DL, get(VC16::LLI), DstReg)
        .addReg(DstReg, RegState::Kill)
        .addImm(Lo5)
        .setMIFlag(Flag);
  }
}
