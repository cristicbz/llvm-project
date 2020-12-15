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


def CSR : CalleeSavedRegs<(add R3, R4, R6)>;

// Needed for implementation of VC16RegisterInfo::getNoPreservedMask()
def CSR_NoRegs : CalleeSavedRegs<(add)>;
