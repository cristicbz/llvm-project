//===-- VC16TargetInfo.cpp - VC16 Target Implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace llvm {
Target &getTheVC16Target() {
  static Target TheVC16Target;
  return TheVC16Target;
}

}

extern "C" void LLVMInitializeVC16TargetInfo() {
  RegisterTarget<Triple::vc16> X(getTheVC16Target(), "vc16",
                                 "TTL Video Computer 16", "VC16");
}
