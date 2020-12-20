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

  BuildMI(MBB, MBBI, DL, get(VC16::LEA), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc))
      .addImm(0);
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
      BuildMI(MBB, MBBI, DL, get(VC16::LEA), DstReg)
          .addReg(DstReg, RegState::Kill)
          .addImm(Lo5)
          .setMIFlag(Flag);
    }
  } else {
    BuildMI(MBB, MBBI, DL, get(VC16::LLI), DstReg).addImm(Lo5).setMIFlag(Flag);
  }
}

static VC16Cond::Code getCCFromOp(unsigned OpCode) {
  switch (OpCode) {
  default:
    llvm_unreachable("unknown branch");
  case VC16::BZ:
    return VC16Cond::Z;
  case VC16::BNZ:
    return VC16Cond::NZ;
  case VC16::BN:
    return VC16Cond::N;
  case VC16::BNN:
    return VC16Cond::NN;
  case VC16::BLT:
    return VC16Cond::LT;
  case VC16::BGE:
    return VC16Cond::GE;
  case VC16::BNC:
    return VC16Cond::NC;
  }
}

static unsigned getOpFromCC(VC16Cond::Code CondCode) {
  switch (CondCode) {
  default:
    llvm_unreachable("unknown branch");
  case VC16Cond::Z:
    return VC16::BZ;
  case VC16Cond::NZ:
    return VC16::BNZ;
  case VC16Cond::N:
    return VC16::BN;
  case VC16Cond::NN:
    return VC16::BNN;
  case VC16Cond::LT:
    return VC16::BLT;
  case VC16Cond::GE:
    return VC16::BGE;
  case VC16Cond::NC:
    return VC16::BNC;
  }
}

// The contents of values added to Cond are not examined outside of
// VC16InstrInfo, giving us flexibility in what to push to it. For VC16, we
// push BranchOpcode, Reg1, Reg2.
static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  // Block ends with fall-through condbranch.
  assert(LastInst.getDesc().isConditionalBranch() &&
         "Unknown conditional branch");
  Target = LastInst.getOperand(0).getMBB();
  Cond.push_back(MachineOperand::CreateImm(getCCFromOp(LastInst.getOpcode())));
}

static VC16Cond::Code oppositeCC(VC16Cond::Code CC) {
  switch (CC) {
  default:
    llvm_unreachable("unknown CC");
  case VC16Cond::GE:
    return VC16Cond::LT;
  case VC16Cond::LT:
    return VC16Cond::GE;
  case VC16Cond::N:
    return VC16Cond::NN;
  case VC16Cond::NN:
    return VC16Cond::N;
  case VC16Cond::Z:
    return VC16Cond::NZ;
  case VC16Cond::NZ:
    return VC16Cond::Z;
  case VC16Cond::NC:
    return VC16Cond::INVALID;
  }
}

MachineBasicBlock *
VC16InstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  assert(MI.getDesc().isBranch() && "Unexpected opcode!");
  // The branch target is always the last operand.
  int NumOp = MI.getNumExplicitOperands();
  return MI.getOperand(NumOp - 1).getMBB();
}

bool VC16InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                  MachineBasicBlock *&TBB,
                                  MachineBasicBlock *&FBB,
                                  SmallVectorImpl<MachineOperand> &Cond,
                                  bool AllowModify) const {
  TBB = FBB = nullptr;
  Cond.clear();

  // If the block has no terminators, it just falls into the block after it.
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end() || !isUnpredicatedTerminator(*I))
    return false;

  // Count the number of terminators and find the first unconditional or
  // indirect branch.
  MachineBasicBlock::iterator FirstUncondOrIndirectBr = MBB.end();
  int NumTerminators = 0;
  for (auto J = I.getReverse(); J != MBB.rend() && isUnpredicatedTerminator(*J);
       J++) {
    NumTerminators++;
    if (J->getDesc().isUnconditionalBranch() ||
        J->getDesc().isIndirectBranch()) {
      FirstUncondOrIndirectBr = J.getReverse();
    }
  }

  // If AllowModify is true, we can erase any terminators after
  // FirstUncondOrIndirectBR.
  if (AllowModify && FirstUncondOrIndirectBr != MBB.end()) {
    while (std::next(FirstUncondOrIndirectBr) != MBB.end()) {
      std::next(FirstUncondOrIndirectBr)->eraseFromParent();
      NumTerminators--;
    }
    I = FirstUncondOrIndirectBr;
  }

  // We can't handle blocks that end in an indirect branch.
  if (I->getDesc().isIndirectBranch())
    return true;

  // We can't handle blocks with more than 2 terminators.
  if (NumTerminators > 2)
    return true;

  // Handle a single unconditional branch.
  if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
    TBB = getBranchDestBlock(*I);
    return false;
  }

  // Handle a single conditional branch.
  if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
    parseCondBranch(*I, TBB, Cond);
    return false;
  }

  // Handle a conditional branch followed by an unconditional branch.
  if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
      I->getDesc().isUnconditionalBranch()) {
    parseCondBranch(*std::prev(I), TBB, Cond);
    FBB = getBranchDestBlock(*I);
    return false;
  }

  // Otherwise, we can't handle this.
  return true;
}

unsigned VC16InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                     int *BytesRemoved) const {
  assert(!BytesRemoved && "Code size not handled");
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return 0;

  if (!I->getDesc().isUnconditionalBranch() &&
      !I->getDesc().isConditionalBranch())
    return 0;

  // Remove the branch.
  I->eraseFromParent();

  I = MBB.end();

  if (I == MBB.begin())
    return 1;
  --I;
  if (!I->getDesc().isConditionalBranch())
    return 1;

  // Remove the branch.
  I->eraseFromParent();
  return 2;
}

MachineInstr &VC16InstrInfo::insertJump(MachineBasicBlock &MBB,
                                        const DebugLoc &DL,
                                        MachineBasicBlock *TBB) const {
  Register ScratchReg =
      MBB.getParent()->getRegInfo().createVirtualRegister(&VC16::GPRRegClass);
  return *BuildMI(&MBB, DL, get(VC16::JAL))
              .addReg(ScratchReg, RegState::Dead | RegState::Define)
              .addMBB(TBB);
}

// Inserts a branch into the end of the specific MachineBasicBlock, returning
// the number of instructions inserted.
unsigned VC16InstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  assert(!BytesAdded && "Code size not handled.");

  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "VC16 branch conditions have two components!");

  // Unconditional branch.
  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    MachineInstr &MI = insertJump(MBB, DL, TBB);
    if (BytesAdded) {
      *BytesAdded = getInstSizeInBytes(MI);
    }
    return 1;
  }

  // Conditional branch.
  VC16Cond::Code CC = (VC16Cond::Code)Cond[0].getImm();
  MachineInstr &CondMI = *BuildMI(&MBB, DL, get(getOpFromCC(CC))).addMBB(TBB);

  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(CondMI);
  //
  // One-way conditional branch.
  if (!FBB)
    return 1;

  // Two-way Conditional branch. Insert the second branch.
  // auto &MI = insertJump(MBB, DL, FBB);
  MachineInstr &MI = insertJump(MBB, DL, FBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(MI);

  return 2;
}

bool VC16InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert((Cond.size() == 1) && "Invalid branch condition!");
  VC16Cond::Code CondCode =
      oppositeCC(static_cast<VC16Cond::Code>(Cond[0].getImm()));
  if (CondCode == VC16Cond::INVALID) {
    return true;
  }
  Cond[0].setImm(CondCode);
  return false;
}
