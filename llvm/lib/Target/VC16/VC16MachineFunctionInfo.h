//=- VC16MachineFunctionInfo.h - VC16 machine function info -------*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares VC16-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_VC16MACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_VC16_VC16MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// VC16MachineFunctionInfo - This class is derived from MachineFunctionInfo
/// and contains private VC16-specific information for each MachineFunction.
class VC16MachineFunctionInfo : public MachineFunctionInfo {

  /// FrameIndex for start of varargs area
  int VarArgsFrameIndex = 0;
  /// Size of the save area used for varargs
  int VarArgsSaveSize = 0;

public:
  VC16MachineFunctionInfo() = default;

  explicit VC16MachineFunctionInfo(MachineFunction &MF) {}

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }

  unsigned getVarArgsSaveSize() const { return VarArgsSaveSize; }
  void setVarArgsSaveSize(int Size) { VarArgsSaveSize = Size; }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_VC16_VC16MACHINEFUNCTIONINFO_H
