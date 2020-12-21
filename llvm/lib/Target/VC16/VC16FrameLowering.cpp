//===-- VC16FrameLowering.cpp - VC16 Frame Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the VC16 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "VC16FrameLowering.h"
#include "VC16MachineFunctionInfo.h"
#include "VC16Subtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"

using namespace llvm;

// Returns the register used to hold the frame pointer.
static Register getFPReg(const VC16Subtarget &STI) { return VC16::X2; }

// Returns the register used to hold the stack pointer.
static Register getSPReg(const VC16Subtarget &STI) { return VC16::X0; }

bool VC16FrameLowering::hasFP(const MachineFunction &MF) const { return true; }

void VC16FrameLowering::emitPrologue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");

  if (!hasFP(MF)) {
    report_fatal_error(
        "emitPrologue doesn't support framepointer-less functions");
  }

  MachineFrameInfo &MFI = MF.getFrameInfo();
  const auto *VCFI = MF.getInfo<VC16MachineFunctionInfo>();
  MachineBasicBlock::iterator MBBI = MBB.begin();

  Register FPReg = getFPReg(STI);
  Register SPReg = getSPReg(STI);

  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc DL;

  // Determine the correct frame layout
  determineFrameLayout(MF);

  // FIXME (note copied from Lanai): This appears to be overallocating.  Needs
  // investigation. Get the number of bytes to allocate from the FrameInfo.
  uint64_t StackSize = MFI.getStackSize();

  // Early exit if there is no need to allocate on the stack
  if (StackSize == 0 && !MFI.adjustsStack())
    return;

  // Allocate space on the stack if necessary.
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, -StackSize, MachineInstr::FrameSetup);

  // The frame pointer is callee-saved, and code has been generated for us to
  // save it to the stack. We need to skip over the storing of callee-saved
  // registers as the frame pointer must be modified after it has been saved
  // to the stack, not before.
  // FIXME: assumes exactly one instruction is used to save each callee-saved
  // register.
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  std::advance(MBBI, CSI.size());

  // Generate new FP.
  adjustReg(MBB, MBBI, DL, FPReg, SPReg, StackSize - VCFI->getVarArgsSaveSize(),
            MachineInstr::FrameSetup);
}

void VC16FrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  if (!hasFP(MF)) {
    report_fatal_error(
        "emitEpilogue doesn't support framepointer-less functions");
  }

  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const VC16RegisterInfo *RI = STI.getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const auto *VCFI = MF.getInfo<VC16MachineFunctionInfo>();
  DebugLoc DL = MBBI->getDebugLoc();
  Register FPReg = getFPReg(STI);
  Register SPReg = getSPReg(STI);

  // Skip to before the restores of callee-saved registers
  // FIXME: assumes exactly one instruction is used to restore each
  // callee-saved register.
  MachineBasicBlock::iterator LastFrameDestroy = MBBI;
  std::advance(LastFrameDestroy, -MFI.getCalleeSavedInfo().size());

  uint64_t StackSize = MFI.getStackSize();

  // Restore the stack pointer using the value of the frame pointer. Only
  // necessary if the stack pointer was modified, meaning the stack size is
  // unknown.
  if (RI->needsStackRealignment(MF) || MFI.hasVarSizedObjects()) {
    adjustReg(MBB, LastFrameDestroy, DL, SPReg, FPReg,
              -StackSize + VCFI->getVarArgsSaveSize(),
              MachineInstr::FrameDestroy);
  }

  // Deallocate stack
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize, MachineInstr::FrameDestroy);
}

int VC16FrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                              Register &FrameReg) const {

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset). We can't use frame pointer due to the lack of
  // negative offsets.
  int Offset = MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
               MFI.getOffsetAdjustment();
  FrameReg = getSPReg(STI);
  Offset += MF.getFrameInfo().getStackSize();
  return Offset;
}

void VC16FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                             BitVector &SavedRegs,
                                             RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  // TODO: Once frame pointer elimination is implemented, don't
  // unconditionally spill the frame pointer and return address.
  SavedRegs.set(VC16::X2);
  SavedRegs.set(VC16::X1);
}

void VC16FrameLowering::processFunctionBeforeFrameFinalized(
    MachineFunction &MF, RegScavenger *RS) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterClass *RC = &VC16::GPRRegClass;
  // estimateStackSize has been observed to under-estimate the final stack
  // size, so give ourselves wiggle-room by checking for stack size
  // representable an 4-bit signed field rather than 5-bits.
  // FIXME: It may be possible to craft a function with a small stack that
  // still needs an emergency spill slot for branch relaxation. This case
  // would currently be missed.
  if (!isUInt<5>(MFI.estimateStackSize(MF))) {
    int RegScavFI = MFI.CreateStackObject(RegInfo->getSpillSize(*RC),
                                          RegInfo->getSpillAlign(*RC), false);
    RS->addScavengingFrameIndex(RegScavFI);
  }
}

// Determines the size of the frame and maximum call frame size.
void VC16FrameLowering::determineFrameLayout(MachineFunction &MF) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const VC16RegisterInfo *RI = STI.getRegisterInfo();

  // Get the number of bytes to allocate from the FrameInfo.
  uint64_t FrameSize = MFI.getStackSize();

  // Get the alignment.
  Align StackAlign =
      RI->needsStackRealignment(MF) ? MFI.getMaxAlign() : getStackAlign();

  // Get the maximum call frame size of all the calls.
  uint64_t MaxCallFrameSize = MFI.getMaxCallFrameSize();

  // If we have dynamic alloca then MaxCallFrameSize needs to be aligned so
  // that allocations will be aligned.
  if (MFI.hasVarSizedObjects())
    MaxCallFrameSize = alignTo(MaxCallFrameSize, StackAlign);

  // Update maximum call frame size.
  MFI.setMaxCallFrameSize(MaxCallFrameSize);

  // Include call frame size in total.
  if (!(hasReservedCallFrame(MF) && MFI.adjustsStack()))
    FrameSize += MaxCallFrameSize;

  // Make sure the frame is aligned.
  FrameSize = alignTo(FrameSize, StackAlign);

  // Update frame info.
  MFI.setStackSize(FrameSize);
}

void VC16FrameLowering::adjustReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  const DebugLoc &DL, unsigned DestReg,
                                  unsigned SrcReg, int64_t Val,
                                  MachineInstr::MIFlag Flag) const {
  MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();
  const VC16InstrInfo *TII = STI.getInstrInfo();

  if (isInt<6>(Val)) {
    if (Val < -16) {
      BuildMI(MBB, MBBI, DL, TII->get(VC16::LEA), DestReg)
          .addReg(SrcReg)
          .addImm(-16)
          .setMIFlag(Flag);
      Val += 16;
      SrcReg = DestReg;
    } else if (Val > 15) {
      BuildMI(MBB, MBBI, DL, TII->get(VC16::LEA), DestReg)
          .addReg(SrcReg)
          .addImm(15)
          .setMIFlag(Flag);
      Val -= 15;
      SrcReg = DestReg;
    }
    if (Val != 0) {
      BuildMI(MBB, MBBI, DL, TII->get(VC16::LEA), DestReg)
          .addReg(SrcReg)
          .addImm(Val)
          .setMIFlag(Flag);
    }
  } else if (isInt<16>(Val)) {
    unsigned Opc = VC16::ADDN;
    unsigned ScratchReg = MRI.createVirtualRegister(&VC16::GPRRegClass);
    TII->movImm16(MBB, MBBI, DL, ScratchReg, Val, Flag);
    BuildMI(MBB, MBBI, DL, TII->get(Opc), DestReg)
        .addReg(DestReg)
        .addReg(ScratchReg, RegState::Kill)
        .setMIFlag(Flag);
  } else {
    report_fatal_error("adjustReg cannot yet handle adjustments >32 bits");
  }
}
