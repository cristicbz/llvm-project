//===-- VC16.h - Top-level interface for VC16 -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// VC16 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_VC16_H
#define LLVM_LIB_TARGET_VC16_VC16_H

#include "MCTargetDesc/VC16BaseInfo.h"

namespace llvm {
class VC16TargetMachine;
class AsmPrinter;
class FunctionPass;
class MCInst;
class MCOperand;
class MachineInstr;
class MachineOperand;

void LowerVC16MachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                   const AsmPrinter &AP);
bool LowerVC16MachineOperandToMCOperand(const MachineOperand &MO,
                                        MCOperand &MCOp, const AsmPrinter &AP);

FunctionPass *createVC16ISelDag(VC16TargetMachine &TM);
}

#endif
