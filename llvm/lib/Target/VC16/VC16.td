//===-- VC16.td - Describe the VC16 Target Machine ---------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register file, instruction descriptions.
//===----------------------------------------------------------------------===//

include "VC16RegisterInfo.td"
include "VC16InstrInfo.td"

def VC16InstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// VC16 processors supported.
//===----------------------------------------------------------------------===//

def : ProcessorModel<"generic-vc16", NoSchedModel, []>;

//===----------------------------------------------------------------------===//
// Define the VC16 target.
//===----------------------------------------------------------------------===//

def VC16 : Target {
  let InstructionSet = VC16InstrInfo;
}
