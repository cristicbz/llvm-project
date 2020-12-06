//===-- VC16AsmParser.cpp - Parse VC16 assembly to MCInst instructions --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/VC16BaseInfo.h"
#include "MCTargetDesc/VC16MCExpr.h"
#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

namespace {
struct VC16Operand;

class VC16AsmParser : public MCTargetAsmParser {
  SMLoc getLoc() const { return getParser().getTok().getLoc(); }

  bool generateImmOutOfRangeError(OperandVector &Operands, uint64_t ErrorInfo,
                                  int Lower, int Upper, Twine Msg);


  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;

// Auto-generated instruction matching functions
#define GET_ASSEMBLER_HEADER
#include "VC16GenAsmMatcher.inc"

  OperandMatchResultTy parseImmediate(OperandVector &Operands);
  OperandMatchResultTy parseRegister(OperandVector &Operands);
  OperandMatchResultTy parseMemOpBaseReg(OperandVector &Operands);
  OperandMatchResultTy parseOperandWithModifier(OperandVector &Operands);

  bool parseOperand(OperandVector &Operands);

public:
  enum VC16MatchResultTy {
    Match_Dummy = FIRST_TARGET_MATCH_RESULT_TY,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "VC16GenAsmMatcher.inc"
#undef GET_OPERAND_DIAGNOSTIC_TYPES
  };

  static bool classifySymbolRef(const MCExpr *Expr,
                                VC16MCExpr::VariantKind &Kind,
                                int64_t &Addend);


  VC16AsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
                 const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII) {
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));
  }
};

/// VC16Operand - Instances of this class represent a parsed machine
/// instruction
struct VC16Operand : public MCParsedAsmOperand {

  enum KindTy {
    Token,
    Register,
    Immediate,
  } Kind;

  struct RegOp {
    unsigned RegNum;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  SMLoc StartLoc, EndLoc;
  union {
    StringRef Tok;
    RegOp Reg;
    ImmOp Imm;
  };

  VC16Operand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

public:
  VC16Operand(const VC16Operand &o) : MCParsedAsmOperand() {
    Kind = o.Kind;
    StartLoc = o.StartLoc;
    EndLoc = o.EndLoc;
    switch (Kind) {
    case Register:
      Reg = o.Reg;
      break;
    case Immediate:
      Imm = o.Imm;
      break;
    case Token:
      Tok = o.Tok;
      break;
    }
  }

  bool isToken() const override { return Kind == Token; }
  bool isReg() const override { return Kind == Register; }
  bool isImm() const override { return Kind == Immediate; }
  bool isMem() const override { return false; }

  bool evaluateConstantImm(int64_t &Imm, VC16MCExpr::VariantKind &VK) const {
    const MCExpr *Val = getImm();
    bool Ret = false;
    if (auto *RE = dyn_cast<VC16MCExpr>(Val)) {
      Ret = RE->evaluateAsConstant(Imm);
      VK = RE->getKind();
    } else if (auto CE = dyn_cast<MCConstantExpr>(Val)) {
      Ret = true;
      VK = VC16MCExpr::VK_VC16_None;
      Imm = CE->getValue();
    }
    return Ret;
  }

  // True if operand is a symbol with no modifiers, or a constant with no
  // modifiers and isShiftedInt<N-1, 1>(Op).
  template <int N> bool isBareSimmNLsb0() const {
    int64_t Imm;
    VC16MCExpr::VariantKind VK;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    bool IsValid;
    if (!IsConstantImm)
      IsValid = VC16AsmParser::classifySymbolRef(getImm(), VK, Imm);
    else
      IsValid = isShiftedInt<N - 1, 1>(Imm);
    return IsValid && VK == VC16MCExpr::VK_VC16_None;
   }

  bool isUImm4() const {
    int64_t Imm;
    VC16MCExpr::VariantKind VK;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    return IsConstantImm && isUInt<4>(Imm) && VK == VC16MCExpr::VK_VC16_None;
  }

  bool isUImm5() const {
    VC16MCExpr::VariantKind VK;
    int64_t Imm;
    bool IsValid;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    if (!IsConstantImm)
      IsValid = VC16AsmParser::classifySymbolRef(getImm(), VK, Imm);
    else
      IsValid = isUInt<5>(Imm);
    return IsValid &&
           (VK == VC16MCExpr::VK_VC16_None || VK == VC16MCExpr::VK_VC16_LO);
  }

  bool isUImm11() const {
    VC16MCExpr::VariantKind VK;
    int64_t Imm;
    bool IsValid;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    if (!IsConstantImm)
      IsValid = VC16AsmParser::classifySymbolRef(getImm(), VK, Imm);
    else
      IsValid = isUInt<11>(Imm);
    return IsValid && (VK == VC16MCExpr::VK_VC16_None ||
                       VK == VC16MCExpr::VK_VC16_HIS ||
                       VK == VC16MCExpr::VK_VC16_HIU);
  }

  bool isSImm5() const {
    VC16MCExpr::VariantKind VK;
    int64_t Imm;
    bool IsValid;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    if (!IsConstantImm)
      IsValid = VC16AsmParser::classifySymbolRef(getImm(), VK, Imm);
    else
      IsValid = isInt<5>(Imm);
    return IsValid &&
           (VK == VC16MCExpr::VK_VC16_None || VK == VC16MCExpr::VK_VC16_LO);
  }

  bool isUImm6Lsb0() const {
    VC16MCExpr::VariantKind VK;
    int64_t Imm;
    bool IsValid;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(Imm, VK);
    if (!IsConstantImm)
      IsValid = VC16AsmParser::classifySymbolRef(getImm(), VK, Imm);
    else
      IsValid = isShiftedUInt<5, 1>(Imm);
    return IsValid &&
           (VK == VC16MCExpr::VK_VC16_None || VK == VC16MCExpr::VK_VC16_LO);
  }

  bool isSImm9Lsb0() const {
    return isBareSimmNLsb0<9>();
  }

  bool isSImm11Lsb0() const {
    return isBareSimmNLsb0<11>();
  }


  /// getStartLoc - Gets location of the first token of this operand
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Gets location of the last token of this operand
  SMLoc getEndLoc() const override { return EndLoc; }

  unsigned getReg() const override {
    assert(Kind == Register && "Invalid type access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert(Kind == Immediate && "Invalid type access!");
    return Imm.Val;
  }

  StringRef getToken() const {
    assert(Kind == Token && "Invalid type access!");
    return Tok;
  }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case Immediate:
      OS << *getImm();
      break;
    case Register:
      OS << "<register x";
      OS << getReg() << ">";
      break;
    case Token:
      OS << "'" << getToken() << "'";
      break;
    }
  }

  static std::unique_ptr<VC16Operand> createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<VC16Operand>(Token);
    Op->Tok = Str;
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<VC16Operand> createReg(unsigned RegNo, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<VC16Operand>(Register);
    Op->Reg.RegNum = RegNo;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<VC16Operand> createImm(const MCExpr *Val, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<VC16Operand>(Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    assert(Expr && "Expr shouldn't be null!");
    int64_t Imm = 0;
    bool IsConstant = false;
    if (auto *RE = dyn_cast<VC16MCExpr>(Expr)) {
      IsConstant = RE->evaluateAsConstant(Imm);
    } else if (auto *CE = dyn_cast<MCConstantExpr>(Expr)) {
      IsConstant = true;
      Imm = CE->getValue();
    }

    if (IsConstant)
      Inst.addOperand(MCOperand::createImm(Imm));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  // Used by the TableGen Code
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }
};
} // end anonymous namespace.

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "VC16GenAsmMatcher.inc"


bool VC16AsmParser::generateImmOutOfRangeError(
    OperandVector &Operands, uint64_t ErrorInfo, int Lower, int Upper,
    Twine Msg = "immediate must be an integer in the range") {
  SMLoc ErrorLoc = ((VC16Operand &)*Operands[ErrorInfo]).getStartLoc();
  return Error(ErrorLoc, Msg + " [" + Twine(Lower) + ", " + Twine(Upper) + "]");
}


bool VC16AsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                             OperandVector &Operands,
                                             MCStreamer &Out,
                                             uint64_t &ErrorInfo,
                                             bool MatchingInlineAsm) {
  MCInst Inst;
  SMLoc ErrorLoc;

  switch (MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm)) {
  default:
    break;
  case Match_Success:
    Inst.setLoc(IDLoc);
    Out.emitInstruction(Inst, getSTI());
    return false;
  case Match_MissingFeature:
    return Error(IDLoc, "instruction use requires an option to be enabled");
  case Match_MnemonicFail:
    return Error(IDLoc, "unrecognized instruction mnemonic");
  case Match_InvalidOperand:
    ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(ErrorLoc, "too few operands for instruction");

      ErrorLoc = ((VC16Operand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  case Match_InvalidSImm11Lsb0:
      return generateImmOutOfRangeError(
          Operands, ErrorInfo, -(1 << 10), (1 << 10) - 2,
          "immediate must be a multiple of 2 bytes in the range");
  case Match_InvalidUImm11:
      return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 11) - 1);
  case Match_InvalidUImm4:
      return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 4) - 1);
  case Match_InvalidSImm5:
      return generateImmOutOfRangeError(Operands, ErrorInfo, -(1 << 4),
                                        (1 << 4) - 1);
  case Match_InvalidUImm5:
      return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 5) - 1);
  case Match_InvalidUImm6Lsb0:
      return generateImmOutOfRangeError(
          Operands, ErrorInfo, 0, (1 << 6) - 2,
          "immediate must be a multiple of 2 bytes in the range");
  case Match_InvalidSImm9Lsb0:
      return generateImmOutOfRangeError(
          Operands, ErrorInfo, -(1 << 8), (1 << 8) - 2,
          "immediate must be a multiple of 2 bytes in the range");
  }

  llvm_unreachable("Unknown match type detected!");
}



bool VC16AsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                   SMLoc &EndLoc) {
  if (tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success)
    return Error(StartLoc, "invalid register name");
  return false;
}

OperandMatchResultTy VC16AsmParser::tryParseRegister(unsigned &RegNo,
                                                     SMLoc &StartLoc,
                                                     SMLoc &EndLoc) {
  const AsmToken &Tok = getParser().getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  StringRef Name = getLexer().getTok().getIdentifier();

  if (!MatchRegisterName(Name) || !MatchRegisterAltName(Name)) {
    getParser().Lex(); // Eat identifier token.
    return MatchOperand_Success;
  }

  return MatchOperand_NoMatch;
}

OperandMatchResultTy VC16AsmParser::parseRegister(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::Identifier:
    StringRef Name = getLexer().getTok().getIdentifier();
    unsigned RegNo = MatchRegisterName(Name);
    if (RegNo == 0) {
      RegNo = MatchRegisterAltName(Name);
      if (RegNo == 0)
        return MatchOperand_NoMatch;
    }
    getLexer().Lex();
    Operands.push_back(VC16Operand::createReg(RegNo, S, E));
  }
  return MatchOperand_Success;
}

OperandMatchResultTy VC16AsmParser::parseImmediate(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
  const MCExpr *Res;

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::String:
    if (getParser().parseExpression(Res))
      return MatchOperand_ParseFail;
    break;
  case AsmToken::Identifier: {
    StringRef Identifier;
    if (getParser().parseIdentifier(Identifier))
      return MatchOperand_ParseFail;
    MCSymbol *Sym = getContext().getOrCreateSymbol(Identifier);
    Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, getContext());
    break;
  }
  case AsmToken::Percent:
    return parseOperandWithModifier(Operands);
  }

  Operands.push_back(VC16Operand::createImm(Res, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy
VC16AsmParser::parseOperandWithModifier(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }
  StringRef Identifier = getParser().getTok().getIdentifier();
  VC16MCExpr::VariantKind VK = VC16MCExpr::getVariantKindForName(Identifier);
  if (VK == VC16MCExpr::VK_VC16_Invalid) {
    Error(getLoc(), "unrecognized operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat the identifier
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }
  getParser().Lex(); // Eat '('

  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *ModExpr = VC16MCExpr::create(SubExpr, VK, getContext());
  Operands.push_back(VC16Operand::createImm(ModExpr, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy
VC16AsmParser::parseMemOpBaseReg(OperandVector &Operands) {
  if (getLexer().isNot(AsmToken::LParen)) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '('
  Operands.push_back(VC16Operand::createToken("(", getLoc()));

  if (parseRegister(Operands) != MatchOperand_Success) {
    Error(getLoc(), "expected register");
    return MatchOperand_ParseFail;
  }

  if (getLexer().isNot(AsmToken::RParen)) {
    Error(getLoc(), "expected ')'");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat ')'
  Operands.push_back(VC16Operand::createToken(")", getLoc()));

  return MatchOperand_Success;
}

/// Looks at a token type and creates the relevant operand
/// from this information, adding to Operands.
/// If operand was parsed, returns false, else true.
bool VC16AsmParser::parseOperand(OperandVector &Operands) {
  // Attempt to parse token as register
  if (parseRegister(Operands) == MatchOperand_Success)
    return false;

  // Attempt to parse token as an immediate
  if (parseImmediate(Operands) == MatchOperand_Success) {
    // Parse memory base register if present
    if (getLexer().is(AsmToken::LParen))
      return parseMemOpBaseReg(Operands) != MatchOperand_Success;
    return false;
  }

  // Finally we have exhausted all options and must declare defeat.
  Error(getLoc(), "unknown operand");
  return true;
}

bool VC16AsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                      StringRef Name, SMLoc NameLoc,
                                      OperandVector &Operands) {
  // First operand is token for instruction
  Operands.push_back(VC16Operand::createToken(Name, NameLoc));

  // If there are no more operands, then finish
  if (getLexer().is(AsmToken::EndOfStatement))
    return false;

  // Parse first operand
  if (parseOperand(Operands))
    return true;

  // Parse until end of statement, consuming commas between operands
  while (getLexer().is(AsmToken::Comma)) {
    // Consume comma token
    getLexer().Lex();

    // Parse next operand
    if (parseOperand(Operands))
      return true;
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    getParser().eatToEndOfStatement();
    return Error(Loc, "unexpected token");
  }

  getParser().Lex(); // Consume the EndOfStatement.
  return false;
}

bool VC16AsmParser::classifySymbolRef(const MCExpr *Expr,
                                       VC16MCExpr::VariantKind &Kind,
                                       int64_t &Addend) {
  Kind = VC16MCExpr::VK_VC16_None;
  Addend = 0;

  if (const VC16MCExpr *RE = dyn_cast<VC16MCExpr>(Expr)) {
    Kind = RE->getKind();
    Expr = RE->getSubExpr();
  }

  // It's a simple symbol reference or constant with no addend.
  if (isa<MCConstantExpr>(Expr) || isa<MCSymbolRefExpr>(Expr))
    return true;

  const MCBinaryExpr *BE = dyn_cast<MCBinaryExpr>(Expr);
  if (!BE)
    return false;

  if (!isa<MCSymbolRefExpr>(BE->getLHS()))
    return false;

  if (BE->getOpcode() != MCBinaryExpr::Add &&
      BE->getOpcode() != MCBinaryExpr::Sub)
    return false;

  // We are able to support the subtraction of two symbol references
  if (BE->getOpcode() == MCBinaryExpr::Sub &&
      isa<MCSymbolRefExpr>(BE->getRHS()))
    return true;

  // See if the addend is is a constant, otherwise there's more going
  // on here than we can deal with.
  auto AddendExpr = dyn_cast<MCConstantExpr>(BE->getRHS());
  if (!AddendExpr)
    return false;

  Addend = AddendExpr->getValue();
  if (BE->getOpcode() == MCBinaryExpr::Sub)
    Addend = -Addend;

  // It's some symbol reference + a constant addend
  return Kind != VC16MCExpr::VK_VC16_Invalid;
}


bool VC16AsmParser::ParseDirective(AsmToken DirectiveID) { return true; }

extern "C" void LLVMInitializeVC16AsmParser() {
  RegisterMCAsmParser<VC16AsmParser> X(getTheVC16Target());
}
