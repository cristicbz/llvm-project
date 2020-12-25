//===- VC16.cpp ----------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {

class VC16 final : public TargetInfo {
public:
  virtual uint32_t calcEFlags() const override;
  RelExpr getRelExpr(RelType type, const Symbol &S,
                     const uint8_t *loc) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;
};

} // end anonymous namespace

uint32_t VC16::calcEFlags() const {
  assert(!objectFiles.empty());
  return cast<ObjFile<ELF32LE>>(objectFiles.front())
      ->getObj()
      .getHeader()
      ->e_flags;
}

RelExpr VC16::getRelExpr(const RelType type, const Symbol &s,
                         const uint8_t *loc) const {
  switch (type) {
  case R_RISCV_NONE:
    return R_NONE;
  case R_VC16_32:
  case R_VC16_64:
  case R_VC16_HI11U:
  case R_VC16_HI11S:
  case R_VC16_LO5_MW:
  case R_VC16_LO5_MB:
  case R_VC16_LO5_RI5:
  case R_VC16_LO5_RRI5:
    return R_ABS;
  case R_VC16_JAL:
  case R_VC16_BRANCH:
    return R_PC;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s));
    return R_NONE;
  }
}

// Extract bits V[Begin:End], where range is inclusive, and Begin must be < 63.
void VC16::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_VC16_32:
    write32le(loc, val);
    return;
  case R_VC16_64:
    write64le(loc, val);
    return;
  case R_VC16_HI11U: {
    const uint16_t insn = read16le(loc) & 0b0011100000000011;
    const uint64_t adjusted = ((val & 0xffff) >> 5);
    const uint64_t msb2 = adjusted & 0b11000000000;
    const uint64_t lsb9 = adjusted & 0b00111111111;
    const uint16_t update = (msb2 << 5) | (lsb9 << 2);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_HI11S: {
    const uint16_t insn = read16le(loc) & 0b0011100000000011;
    const uint64_t adjusted = ((((val & 0xffff) + (1 << 4))) >> 5);
    const uint64_t msb2 = adjusted & 0b11000000000;
    const uint64_t lsb9 = adjusted & 0b00111111111;
    const uint16_t update = (msb2 << 5) | (lsb9 << 2);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_LO5_MB: {
    const uint16_t insn = read16le(loc) & 0b0011111111000111;
    const uint64_t msb2 = val & 0b11000000000;
    const uint64_t lsb9 = val & 0b00111111111;
    const uint16_t update = (msb2 << 11) | (lsb9 << 3);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_LO5_MW: {
    if (val & 0x1) {
      error(getErrorLocation(loc) +
            "unaligned MW relocation: " + toString(val));
    }
    const uint16_t insn = read16le(loc) & 0b0011111111000111;
    const uint64_t adjusted = (val & 0x1f) >> 1;
    const uint64_t msb2 = adjusted & 0b11000000000;
    const uint64_t lsb9 = adjusted & 0b00111111111;
    const uint16_t update = (msb2 << 11) | (lsb9 << 3);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_LO5_RI5: {
    const uint16_t insn = read16le(loc) & 0b0011100011111111;
    const uint64_t msb2 = val & 0b11000;
    const uint64_t lsb3 = val & 0b00111;
    const uint16_t update = (msb2 << 11) | (lsb3 << 8);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_LO5_RRI5: {
    const uint16_t insn = read16le(loc) & 0b0011111100011111;
    const uint64_t msb2 = val & 0b11000;
    const uint64_t lsb3 = val & 0b00111;
    const uint16_t update = (msb2 << 11) | (lsb3 << 5);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_JAL: {
    if (!isInt<11>(val))
      error(getErrorLocation(loc) +
            "jal reolcation is out of range: " + toString(val));
    if (val & 0x1)
      error(getErrorLocation(loc) +
            "jal relocation is not 2-byte aligned: " + toString(val));
    const uint16_t insn = read16le(loc) & 0b0011100000000111;
    const uint64_t adjusted = val >> 1;
    const uint64_t msb2 = adjusted & 0b1100000000;
    const uint64_t lsb8 = adjusted & 0b0011111111;
    const uint16_t update = (msb2 << 6) | (lsb8 << 3);
    write16le(loc, insn | update);
    return;
  }
  case R_VC16_BRANCH: {
    if (!isInt<9>(val))
      error(getErrorLocation(loc) +
            "branch reolcation is out of range: " + toString(val));
    if (val & 0x1)
      error(getErrorLocation(loc) +
            "branch relocation is not 2-byte aligned: " + toString(val));
    const uint16_t insn = read16le(loc) & 0b0000000011111111;
    const uint16_t update = ((val >> 1) & 0xff) << 8;
    write16le(loc, insn | update);
    return;
  }

  default:
    error(getErrorLocation(loc) +
          "unimplemented relocation: " + toString(rel.type));
    return;
  }
}

TargetInfo *elf::getVC16TargetInfo() {
  static VC16 Target;
  return &Target;
}
