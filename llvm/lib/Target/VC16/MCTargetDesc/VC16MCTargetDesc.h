//===-- VC16MCTargetDesc.h - VC16 Target Descriptions ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides VC16 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCTARGETDESC_H
#define LLVM_LIB_TARGET_VC16_MCTARGETDESC_VC16MCTARGETDESC_H

#include "llvm/Config/config.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Support/DataTypes.h"
#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class StringRef;
class Target;
class Triple;
class raw_ostream;
class raw_pwrite_stream;

Target &getTheVC16Target();

MCCodeEmitter *createVC16MCCodeEmitter(const MCInstrInfo &MCII,
                                        const MCRegisterInfo &MRI,
                                        MCContext &Ctx);

MCAsmBackend *createVC16AsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                    const MCRegisterInfo &MRI,
                                    const MCTargetOptions &Options);

std::unique_ptr<MCObjectTargetWriter> createVC16ELFObjectWriter(uint8_t OSABI);
}

// Defines symbolic names for VC16 registers.
#define GET_REGINFO_ENUM
#include "VC16GenRegisterInfo.inc"

// Defines symbolic names for VC16 instructions.
#define GET_INSTRINFO_ENUM
#include "VC16GenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "VC16GenSubtargetInfo.inc"

#endif
