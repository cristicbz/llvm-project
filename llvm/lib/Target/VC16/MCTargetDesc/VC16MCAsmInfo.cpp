//===-- VC16MCAsmInfo.cpp - VC16 Asm properties ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the VC16MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "VC16MCAsmInfo.h"
#include "llvm/ADT/Triple.h"
using namespace llvm;

void VC16MCAsmInfo::anchor() {}

VC16MCAsmInfo::VC16MCAsmInfo(const Triple &TT) {
  CodePointerSize = CalleeSaveStackSlotSize = 2;
  CommentString = ";";
  AlignmentIsInBytes = false;
  SupportsDebugInformation = true;
}
