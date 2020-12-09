//===-- VC16CallingConv.td - Calling Conventions VC16 ----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the VC16 architecture.
//
//===----------------------------------------------------------------------===//

// VC16 16-bit C return-value convention.
def RetCC_VC16: CallingConv<[CCIfType<[i16], CCAssignToReg<[R0, R1]>>]>;

// VC16 32-bit C Calling convention.
def CC_VC16: CallingConv<[
  // Promote i8 args to i16
  CCIfType<[ i8 ], CCPromoteToType<i16>>,

  // All arguments get passed in integer registers if there is space.
  CCIfType<[i16], CCAssignToReg<[ R0, R1, R2 ]>>,

  // Could be assigned to the stack in 2-byte aligned units, but unsupported
  CCAssignToStack<2, 2>
]>;

def CSR : CalleeSavedRegs<(add R3, R4, R6)>;

// Needed for implementation of VC16RegisterInfo::getNoPreservedMask()
def CSR_NoRegs : CalleeSavedRegs<(add)>;
