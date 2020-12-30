//===-- VC16InstrInfo.td - Target Description for VC16 -----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the VC16 instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

include "VC16InstrFormats.td"

//===----------------------------------------------------------------------===//
// VC16 specific DAG Nodes.
//===----------------------------------------------------------------------===//

def SDT_VC16Call         : SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>;
def SDT_VC16CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i16>,
                                            SDTCisVT<1, i16>]>;
def SDT_VC16CallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i16>,
                                         SDTCisVT<1, i16>]>;
def SDT_VC16SelectCC     : SDTypeProfile<1, 4, [SDTCisSameAs<0, 3>,
                                                SDTCisSameAs<3, 4>,
                                                SDTCisVT<0, i16>,
                                                SDTCisVT<1, i16>]>;

// Unary and binary operator instructions that set EFLAGS as a side-effect.
def SDTUnaryArithWithFlags : SDTypeProfile<2, 1,
                                           [SDTCisSameAs<0, 2>,
                                            SDTCisInt<0>, SDTCisVT<1, i16>]>;

def SDTBinaryArithWithFlags : SDTypeProfile<2, 2,
                                            [SDTCisSameAs<0, 2>,
                                             SDTCisSameAs<0, 3>,
                                             SDTCisInt<0>, SDTCisVT<1, i16>]>;

// SDTBinaryArithWithFlagsInOut - RES1, EFLAGS = op LHS, RHS, EFLAGS
def SDTBinaryArithWithFlagsInOut : SDTypeProfile<2, 3,
                                            [SDTCisSameAs<0, 2>,
                                             SDTCisSameAs<0, 3>,
                                             SDTCisInt<0>,
                                             SDTCisVT<1, i16>,
                                             SDTCisVT<4, i16>]>;


// (target, condition code, flags)
def SDT_VC16Brcond : SDTypeProfile<0, 3,
                                  [SDTCisVT<0, OtherVT>,
                                   SDTCisVT<1, i16>,
                                   SDTCisVT<2, i16>]>;
def SDT_VC16Cmp : SDTypeProfile<1, 2, [SDTCisSameAs<1, 2>, SDTCisVT<0, i16>]>;


def Call         : SDNode<"VC16ISD::CALL", SDT_VC16Call,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                           SDNPVariadic]>;
def CallSeqStart : SDNode<"ISD::CALLSEQ_START", SDT_VC16CallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def CallSeqEnd   : SDNode<"ISD::CALLSEQ_END", SDT_VC16CallSeqEnd,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def RetFlag      : SDNode<"VC16ISD::RET_FLAG", SDTNone,
                          [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def SelectCC     : SDNode<"VC16ISD::SELECT_CC", SDT_VC16SelectCC>;

def VC16brcond   : SDNode<"VC16ISD::BRCOND", SDT_VC16Brcond, [SDNPHasChain]>;
def VC16cmp      : SDNode<"VC16ISD::CMP", SDT_VC16Cmp>;

def ccode : Operand<i16> {
  let PrintMethod = "printCondCode";
  let OperandNamespace = "VC16";
  let OperandType = "OPERAND_COND_CODE";
}

//===----------------------------------------------------------------------===//
// Operand and SDNode transformation definitions.
//===----------------------------------------------------------------------===//
class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix # "Imm" # width # suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

class SImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"S", width, suffix> {
}

class UImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"U", width, suffix> {
}

def simm11_lsb0 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<11, "Lsb0">;
  let EncoderMethod = "getImmOpValueAsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<11>";
}
def uimm11 : Operand<i16>, ImmLeaf<i16, [{return isUInt<11>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<11>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<11>";
}
def uimm4 : Operand<i16>, ImmLeaf<i16, [{return isUInt<4>(Imm);}]>  {
  let ParserMatchClass = UImmAsmOperand<4>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<4>";
}
def uimm6 : Operand<i16>, ImmLeaf<i16, [{return isUInt<6>(Imm);}]>  {
  let ParserMatchClass = UImmAsmOperand<6>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<6>";
}
def simm5 : Operand<i16>, ImmLeaf<i16, [{return isInt<5>(Imm);}]>  {
  let ParserMatchClass = SImmAsmOperand<5>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<5>";
}
def uimm5 : Operand<i16>, ImmLeaf<i16, [{return isUInt<5>(Imm);}]>  {
  let ParserMatchClass = UImmAsmOperand<5>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<5>";
}
def uimm6_lsb0 : Operand<i16>, ImmLeaf<i16, [{return isShiftedUInt<5, 1>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<6, "Lsb0">;
  let EncoderMethod = "getImmOpValueLsr1";
  let DecoderMethod = "decodeUImmOperandAndLsl1<6>";
}
def simm9_lsb0 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<9, "Lsb0">;
  let EncoderMethod = "getImmOpValueAsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<9>";
}

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Unary_RF_R<bits<2> op2, bits<8> op8, string opcodestr,
                     list<dag> pattern = []>
    : VCInstRR<op2, op8, (outs GPR:$r1), (ins GPR:$r2),
               opcodestr>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Binary_F_RR<bits<2> op2, bits<8> op8, string opcodestr,
                      list<dag> pattern = []>
    : VCInstRR<op2, op8, (outs), (ins GPR:$r1, GPR:$r2),
               opcodestr, pattern>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Binary_R_RR<bits<2> op2, bits<8> op8, string opcodestr,
                       list<dag> pattern = []>
    : VCInstRR<op2, op8, (outs GPR:$rd), (ins GPR:$r1, GPR:$r2),
               opcodestr, pattern> {
  let Constraints = "$rd = $r1";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Binary_RF_RR<bits<2> op2, bits<8> op8, string opcodestr,
                       list<dag> pattern = []>
    : VCInstRR<op2, op8, (outs GPR:$rd), (ins GPR:$r1, GPR:$r2),
               opcodestr, pattern> {
  let Constraints = "$rd = $r1";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Binary_RF_RI5<bits<8> op8, string opcodestr, list<dag> pattern = []>
    : VCInstRI5<op8, (outs GPR:$rd), (ins GPR:$rs, simm5:$imm5),
                opcodestr, "$rs, $imm5", pattern> {
  let Constraints = "$rd = $rs";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Binary_R_RI5<bits<8> op8, string opcodestr, list<dag> pattern = []>
    : VCInstRI5<op8, (outs GPR:$rd), (ins GPR:$rs, simm5:$imm5),
                opcodestr, "$rs, $imm5", pattern> {
  let Constraints = "$rd = $rs";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Unary_R_RI5<bits<8> op8, string opcodestr, list<dag> pattern = []>
    : VCInstRI5<op8, (outs GPR:$rs), (ins simm5:$imm5),
                opcodestr, "$rs, $imm5", pattern>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Binary_F_RI5<bits<8> op8, string opcodestr, list<dag> pattern = []>
    : VCInstRI5<op8, (outs), (ins GPR:$rs, simm5:$imm5),
                opcodestr, "$rs, $imm5", pattern>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, Defs = [FLAGS] in
class ALU_Binary_F_RI4<bit op1, bits<8> op8, string opcodestr,
                       list<dag> pattern = []>
    : VCInstRI4<op1, op8, (outs), (ins GPR:$rs, uimm4:$imm4),
                opcodestr, "$rs, $imm4">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Binary_R_RI4<bit op1, bits<8> op8, string opcodestr,
                       list<dag> pattern = []>
    : VCInstRI4<op1, op8, (outs GPR:$rd), (ins GPR:$rs, uimm4:$imm4),
                opcodestr, "$rs, $imm4"> {
  let Constraints = "$rd = $rs";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
let isBranch = 1, isTerminator = 1, Uses=[FLAGS] in
class BranchCC_i<bits<8> op8, string opcodestr, bits<8> cond>
    : VCInstB<op8, (outs), (ins simm9_lsb0:$imm8),
              opcodestr, "$imm8", [(VC16brcond bb:$imm8, (i16 cond), FLAGS)]>;

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//
let hasSideEffects = 1, mayStore = 1, mayLoad = 0 in {
def CSRI : VCInstSI6<0b00,   0b010111, (outs SSREG:$seg), (ins uimm6:$imm6),
                     "csri", "$seg, $imm6">;
def CSRR : VCInstCR <0b01, 0b00010111, (outs GPR:$r), (ins CSREG:$csr),
                     "csrr", "$r, $csr">;
def CSRW : VCInstCR <0b01, 0b01010111, (outs CSREG:$csr), (ins GPR:$r),
                     "csrw", "$csr, $r">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
let isAsCheapAsAMove = 1 in
def LUI  : VCInstRI11<0b00, (outs GPR:$rd), (ins uimm11:$imm11),
                      "lui", "$rd, $imm11">;

let isCall = 1 in
def JAL  : VCInstRI10<0b011, (outs GPR_JAL:$rd), (ins simm11_lsb0:$imm10),
                      "jal", "$rd, $imm10">;

let isBarrier = 1, isBranch = 1, isTerminator = 1, rd = 0 in
def   J  : VCInstRI10<0b011, (outs), (ins simm11_lsb0:$imm10),
                      "j", "$imm10", [(br bb:$imm10)]>;

} // hasSideEffects = 0, mayLoad = 0, mayStore = 0


let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
def LW : VCInstMW<0b001, (outs GPR:$rs2),
                  (ins GPR:$rs1, uimm6_lsb0:$imm5, SSREG:$seg),
                  "lw", "$rs2, ${imm5}(${rs1}), ${seg}">;

def LB : VCInstMB<0b101, (outs GPR:$rs2),
                  (ins GPR:$rs1, uimm5:$imm5, SSREG:$seg),
                  "lb", "$rs2, ${imm5}(${rs1}), ${seg}">;

} // hasSideEffects = 0, mayLoad = 1, mayStore = 0

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
def SW  : VCInstMW<0b010, (outs),
                   (ins GPR:$rs2, GPR:$rs1, uimm6_lsb0:$imm5, SSREG:$seg),
                   "sw", "$rs2, ${imm5}(${rs1}), ${seg}">;

def SB  : VCInstMB<0b110, (outs),
                   (ins GPR:$rs2, GPR:$rs1, uimm5:$imm5, SSREG:$seg),
                   "sb", "$rs2, ${imm5}(${rs1}), ${seg}">;
} // hasSideEffects = 0, mayLoad = 0, mayStore = 1

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
let isCommutable = 1, isAsCheapAsAMove = 1 in
def LEA : VCInstRRI5<0b00111, (outs GPR:$rd), (ins GPR:$rs, simm5:$imm5),
                     "lea", "$rd, ${rs}, ${imm5}">;

/// Branch instructions.
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1 in
let Constraints = "$rd = $rs" in
def JALR : VCInstRI5 <0b00001111,
                      (outs GPR:$rd), (ins GPR:$rs, simm5:$imm5),
                      "jalr", "$rs, $imm5">;


def  BZ  : BranchCC_i<0b00101111, "bz", 1>;
def BNZ  : BranchCC_i<0b01001111, "bnz", 2>;
def  BC  : BranchCC_i<0b01101111, "bc", 3>;
def BNC  : BranchCC_i<0b10001111, "bnc", 4>;
def BLT  : BranchCC_i<0b10101111, "blt", 5>;
def BGE  : BranchCC_i<0b11001111, "bge", 6>;
def BNN  : BranchCC_i<0b11101111, "bnn", 7>;

/// ALU RR instructions.
let isCommutable = 1 in
def  MUL :  ALU_Binary_R_RR<0b11, 0b00010111, "mul">;
let isCommutable = 1 in
def MLHU :  ALU_Binary_R_RR<0b11, 0b01010111, "mlhu">;
let isCommutable = 1 in
def MLHS :  ALU_Binary_R_RR<0b11, 0b10010111, "mlhs">;
def  NEG :   ALU_Unary_RF_R<0b11, 0b11010111, "neg">;

let isCommutable = 1 in
def  ADD : ALU_Binary_RF_RR<0b00, 0b00110111, "add">;
def  SUB : ALU_Binary_RF_RR<0b00, 0b01110111, "sub">;
let isCommutable = 1 in
def  XOR : ALU_Binary_RF_RR<0b00, 0b10110111, "xor">;
def  RSB : ALU_Binary_RF_RR<0b00, 0b11110111, "rsb">;

let isCommutable = 1 in
def  ADC : ALU_Binary_RF_RR<0b01, 0b00110111, "adc">;
def  SBC : ALU_Binary_RF_RR<0b01, 0b01110111, "sbc">;
def  SRA :  ALU_Binary_R_RR<0b01, 0b10110111, "sra">;
let isCommutable = 1 in
def   OR : ALU_Binary_RF_RR<0b01, 0b11110111, "or">;

def  SLL :  ALU_Binary_R_RR<0b10, 0b00110111, "sll">;
def  SRL :  ALU_Binary_R_RR<0b10, 0b10110111, "srl">;
let isCommutable = 1 in
def  AND : ALU_Binary_RF_RR<0b10, 0b11110111, "and">;

let isCommutable = 1 in
def ADDN : ALU_Binary_RF_RR<0b11, 0b00110111, "addn">;
def  NOT :   ALU_Unary_RF_R<0b11, 0b10110111, "not">;

/// ALU Compare RR instructions.
let isCompare = 1 in
def CMP :  ALU_Binary_F_RR<0b10, 0b01110111, "cmp",
                           [(set FLAGS, (VC16cmp GPR:$r1, GPR:$r2))]>;
let isCompare = 1 in
def CMPC :  ALU_Binary_F_RR<0b11, 0b01110111, "cmpc", []>;
let isCompare = 1, isCommutable = 1 in
def TEST :  ALU_Binary_F_RR<0b11, 0b11110111, "test",
                            [(set FLAGS, (VC16cmp (and GPR:$r1, GPR:$r2), 0))]>;

/// ALU RI instructions.
def SLLI :   ALU_Binary_R_RI4<0b1, 0b00011111, "slli">;
def SRAI :   ALU_Binary_R_RI4<0b0, 0b10011111, "srai">;
def SRLI :   ALU_Binary_R_RI4<0b1, 0b10011111, "srli">;

let isAsCheapAsAMove = 1 in
def ADDI :  ALU_Binary_RF_RI5<     0b01011111, "addi">;
def ADCI :  ALU_Binary_RF_RI5<     0b11011111, "adci">;
let isAsCheapAsAMove = 1 in
def  ORI :  ALU_Binary_RF_RI5<     0b00111111, "ori">;
let isAsCheapAsAMove = 1 in
def  LLI :    ALU_Unary_R_RI5<     0b10111111, "lli">;
def ANDI :  ALU_Binary_RF_RI5<     0b11111111, "andi">;

/// ALU Compare RI instructions.
let isCompare = 1 in
def TSTI :   ALU_Binary_F_RI4<0b0, 0b00011111, "tsti",
                              [(set FLAGS,
                                (VC16cmp (and GPR:$rs, uimm4:$imm4), 0))]>;

let isCompare = 1 in
def CMPI :   ALU_Binary_F_RI5<     0b01111111, "cmpi",
                              [(set FLAGS, (VC16cmp GPR:$rs, simm5:$imm5))]>;


//===----------------------------------------------------------------------===//
// Pseudo-instructions and codegen patterns
//
// Naming convention: For 'generic' pattern classes, we use the naming
// convention PatTy1Ty2. For pattern classes which offer a more complex
// expension, prefix the class name, e.g. BccPat.
//===----------------------------------------------------------------------===//

/// Generic pattern classes

class PatGpr<SDPatternOperator OpNode, VCInstRR Inst>
    : Pat<(OpNode GPR:$rs), (Inst GPR:$rs)>;
class PatGprGpr<SDPatternOperator OpNode, VCInstRR Inst>
    : Pat<(OpNode GPR:$rd, GPR:$rs), (Inst GPR:$rd, GPR:$rs)>;
class PatSwappedGprGpr<SDPatternOperator OpNode, VCInstRR Inst>
    : Pat<(OpNode GPR:$rs, GPR:$rd), (Inst GPR:$rd, GPR:$rs)>;
class PatGprSimm5<SDPatternOperator OpNode, VCInst Inst>
    : Pat<(OpNode GPR:$rs, simm5:$imm5), (Inst GPR:$rs, simm5:$imm5)>;
class PatGprUimm4<SDPatternOperator OpNode, VCInstRI4 Inst>
    : Pat<(OpNode GPR:$rs, uimm4:$imm4), (Inst GPR:$rs, uimm4:$imm4)>;

// ALU binary

def : PatGprGpr<mul, MUL>;
def : PatGprGpr<mulhu, MLHU>;
def : PatGprGpr<mulhs, MLHS>;
def : PatGpr<ineg, NEG>;

def : PatGprGpr<add, ADD>;
def : PatGprGpr<sub, SUB>;
def : PatGprGpr<xor, XOR>;
def : PatSwappedGprGpr<sub, RSB>;

def : PatGprGpr<adde, ADC>;
def : PatGprGpr<sube, SBC>;
def : PatGprGpr<sra, SRA>;
def : PatGprGpr<or,  OR>;

def : PatGprGpr<shl, SLL>;
def : PatGprGpr<srl, SRL>;
def : PatGprGpr<and, AND>;

def : PatGprGpr<add, ADDN>;
def : PatGpr<not, NOT>;

// ALU unary
def : Pat<(ineg GPR:$rs), (NEG GPR:$rs)>;
def : Pat<(not GPR:$rs), (NOT GPR:$rs)>;

// ALU immediate
def : PatGprSimm5<add, LEA>;

def : PatGprUimm4<shl, SLLI>;
def : PatGprUimm4<sra, SRAI>;
def : PatGprUimm4<srl, SRLI>;

def : PatGprSimm5<adde, ADCI>;
def : PatGprSimm5<or,  ORI>;
def : PatGprSimm5<and, ANDI>;

// Comparison patterns.
def : Pat<(VC16cmp (and GPR:$r1, GPR:$r2), 0),     (TEST GPR:$r1, GPR:$r2)>;
def : Pat<(VC16cmp (and GPR:$rs, uimm4:$imm4), 0), (TSTI GPR:$rs, uimm4:$imm4)>;
def : Pat<(VC16cmp GPR:$r1, simm5:$imm5),          (CMPI GPR:$r1, simm5:$imm5)>;

// Codegen-only immleafs.
def uimm16 : ImmLeaf<i16, [{return isUInt<16>(Imm);}]>;
def simm16 : ImmLeaf<i16, [{return isInt<16>(Imm);}]>;
def simm16_shift5 : ImmLeaf<i16, [{return isShiftedInt<11, 5>(Imm);}]>;
def uimm16_shift5 : ImmLeaf<i16, [{return isShiftedUInt<11, 5>(Imm);}]>;

// Addressing modes.
// Necessary because a frameindex can't be matched directly in a pattern.
def AddrFI : ComplexPattern<iPTR, 1, "SelectAddrFI", [frameindex], []>;

// Extract least significant 12 bits from an immediate value and sign extend
// them.
def LO5Sext : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(SignExtend64<5>(N->getZExtValue()),
                                   SDLoc(N), N->getValueType(0));
}]>;

def LO5Zext : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getZExtValue() & 0b11111,
                                   SDLoc(N), N->getValueType(0));
}]>;

// Extract the most significant 11 bits from an unsigned immediate value.
def HI20U : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((N->getZExtValue() >> 5) & 0x7ff,
                                   SDLoc(N), N->getValueType(0));
}]>;

// Extract the most significant 11 bits from an signed immediate value. Add 1
// if bit 4 is 1, to compensate for the low 11 bits in the matching immediate
// `addi` being negative.
def HI20S : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(
    (((N->getZExtValue() + (1 << 4)) >> 5)) & 0x7ff,
    SDLoc(N),
    N->getValueType(0)
  );
}]>;

/// Predicates
def IsOrAdd: PatFrag<(ops node:$A, node:$B), (or node:$A, node:$B), [{
  return isOrEquivalentToAdd(N);
}]>;


/// Immediates.
def : Pat<(simm5:$imm), (LLI simm5:$imm)>;
def : Pat<(simm16_shift5:$imm), (LUI (HI20U $imm))>;
def : Pat<(simm16:$imm), (LEA (LUI (HI20S imm:$imm)), (LO5Sext imm:$imm))>;


let usesCustomInserter = 1, Uses = [FLAGS] in
def Select_GPR_Using_CC_GPR
    : Pseudo<(outs GPR:$dst),
             (ins i16imm:$imm, GPR:$src, GPR:$src2),
             [(set GPR:$dst,
               (SelectCC (i16 imm:$imm), FLAGS, GPR:$src, GPR:$src2))]>;


/// Branches and jumps
let isBarrier = 1, isBranch = 1, isIndirectBranch = 1, isTerminator = 1 in
def PseudoBRIND : Pseudo<(outs GPR:$rd), (ins GPR:$rs, simm5:$imm5), []>,
                  PseudoInstExpansion<(JALR GPR:$rd, GPR:$rs, simm5:$imm5)> {
    let Constraints = "$rd = $rs";
}

def : Pat<(brind GPR:$rs), (PseudoBRIND GPR:$rs, 0)>;
def : Pat<(brind (add GPR:$rs, simm5:$imm5)),
          (PseudoBRIND GPR:$rs, simm5:$imm5)>;


let isCall = 1, Defs=[X1], Uses=[X1] in
def PseudoCALL : Pseudo<(outs), (ins), [(Call X1)]>,
                 PseudoInstExpansion<(JALR X1, X1, 0)>;

let isBarrier = 1, isReturn = 1, isTerminator = 1 in
def PseudoRET : Pseudo<(outs), (ins), [(RetFlag)]>,
                PseudoInstExpansion<(JALR X1, X1, 0)>;

/// Address space pattern fragments.
foreach as = [ 0, 1, 2, 3 ] in {
let AddressSpaces = [ as ] in {
def load_as_#as : PatFrag<(ops node:$ptr), (load node:$ptr)> {
  let IsLoad = 1;
  let IsNonExtLoad = 1;
}

def extloadi8_as_#as : PatFrag<(ops node:$ptr), (extloadi8 node:$ptr)> {
  let IsLoad = 1;
  let MemoryVT = i8;
}

def zextloadi8_as_#as : PatFrag<(ops node:$ptr), (zextloadi8 node:$ptr)> {
  let IsLoad = 1;
  let MemoryVT = i8;
}

def store_as_#as : PatFrag<(ops node:$val, node:$ptr),
                           (store node:$val, node:$ptr)> {
  let IsStore = 1;
  let IsTruncStore = 0;
}

def truncstorei8_as_#as : PatFrag<(ops node:$val, node:$ptr),
                                  (truncstorei8 node:$val, node:$ptr)> {
  let IsStore = 1;
  let IsTruncStore = 1;
  let MemoryVT = i8;
}
} // let AddressSpaces
} // foreach

/// Loads
multiclass LdPat<PatFrag LoadOp, ImmLeaf LoImmT, VCInst Inst, VC16Reg Seg> {
  def : Pat<(LoadOp GPR:$rs1), (Inst GPR:$rs1, 0, Seg)>;
  def : Pat<(LoadOp AddrFI:$rs1), (Inst AddrFI:$rs1, 0, Seg)>;
  def : Pat<(LoadOp (add GPR:$rs1, LoImmT:$imm5)),
            (Inst GPR:$rs1, LoImmT:$imm5, Seg)>;
  def : Pat<(LoadOp (add AddrFI:$rs1, LoImmT:$imm5)),
            (Inst AddrFI:$rs1, LoImmT:$imm5, Seg)>;
  def : Pat<(LoadOp (IsOrAdd AddrFI:$rs1, LoImmT:$imm5)),
            (Inst AddrFI:$rs1, LoImmT:$imm5, Seg)>;
}

defm : LdPat<extloadi8_as_0, uimm5, LB, SS>;
defm : LdPat<zextloadi8_as_0, uimm5, LB, SS>;
defm : LdPat<load_as_0, uimm6_lsb0, LW, SS>;

defm : LdPat<extloadi8_as_1, uimm5, LB, XS>;
defm : LdPat<zextloadi8_as_1, uimm5, LB, XS>;
defm : LdPat<load_as_1, uimm6_lsb0, LW, XS>;

defm : LdPat<extloadi8_as_2, uimm5, LB, YS>;
defm : LdPat<zextloadi8_as_2, uimm5, LB, YS>;
defm : LdPat<load_as_2, uimm6_lsb0, LW, YS>;

defm : LdPat<extloadi8_as_3, uimm5, LB, CS>;
defm : LdPat<zextloadi8_as_3, uimm5, LB, CS>;
defm : LdPat<load_as_3, uimm6_lsb0, LW, CS>;

/// Stores
multiclass StPat<PatFrag StoreOp, ImmLeaf LoImmT, VCInst Inst, VC16Reg Seg> {
  def : Pat<(StoreOp GPR:$rs2, GPR:$rs1), (Inst GPR:$rs2, GPR:$rs1, 0, Seg)>;
  def : Pat<(StoreOp GPR:$rs2, AddrFI:$rs1), (Inst GPR:$rs2, AddrFI:$rs1, 0, Seg)>;
  def : Pat<(StoreOp GPR:$rs2, (add GPR:$rs1, LoImmT:$imm5)),
            (Inst GPR:$rs2, GPR:$rs1, LoImmT:$imm5, Seg)>;
  def : Pat<(StoreOp GPR:$rs2, (add AddrFI:$rs1, LoImmT:$imm12)),
            (Inst GPR:$rs2, AddrFI:$rs1, LoImmT:$imm12, Seg)>;
  def : Pat<(StoreOp GPR:$rs2, (IsOrAdd AddrFI:$rs1, LoImmT:$imm12)),
            (Inst GPR:$rs2, AddrFI:$rs1, LoImmT:$imm12, Seg)>;
}

defm : StPat<truncstorei8_as_0, uimm5, SB, SS>;
defm : StPat<store_as_0, uimm6_lsb0, SW, SS>;

defm : StPat<truncstorei8_as_1, uimm5, SB, XS>;
defm : StPat<store_as_1, uimm6_lsb0, SW, XS>;

defm : StPat<truncstorei8_as_2, uimm5, SB, YS>;
defm : StPat<store_as_2, uimm6_lsb0, SW, YS>;

defm : StPat<truncstorei8_as_3, uimm5, SB, CS>;
defm : StPat<store_as_3, uimm6_lsb0, SW, CS>;

/// FrameIndex calculations

def : Pat<(add (i16 AddrFI:$rs), simm5:$imm5),
           (LEA (i16 AddrFI:$rs), simm5:$imm5)>;
def : Pat<(IsOrAdd (i16 AddrFI:$rs), simm5:$imm5),
          (LEA (i16 AddrFI:$rs), simm5:$imm5)>;

/// Other pseudo-instructions

// Pessimistically assume the stack pointer will be clobbered
let Defs = [X0], Uses = [X0] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                              [(CallSeqStart timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP   : Pseudo<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                              [(CallSeqEnd timm:$amt1, timm:$amt2)]>;
} // Defs = [X0], Uses = [X0]

/// Aliases
def : InstAlias<"nop",                     (LEA       X0,       X0,       0)>;
def : InstAlias<"mv $rd, $rs",             (LEA       GPR:$rd,  GPR:$rs,  0)>;

def : InstAlias<"jal $offset",             (JAL       X1,       simm11_lsb0:$offset)>;
def : InstAlias<"jr ${rs}",                (JALR      GPR:$rs,  0)>;
def : InstAlias<"ret",                     (JALR      X1,       0), 2>;


def : InstAlias<"lb $rd, ${imm5}(${rs1})",
                (LB  GPR:$rd, GPR:$rs1, uimm5:$imm5, SS)>;
def : InstAlias<"lw $rd, ${imm5}(${rs1})",
                (LW  GPR:$rd, GPR:$rs1, uimm6_lsb0:$imm5, SS)>;
def : InstAlias<"sb $rs2, ${imm5}(${rs1})",
                (SB  GPR:$rs2, GPR:$rs1, uimm5:$imm5, SS)>;
def : InstAlias<"sw $rs2, ${imm5}(${rs1})",
                (SW  GPR:$rs2, GPR:$rs1, uimm6_lsb0:$imm5, SS)>;

let EmitPriority = 0 in {
def : InstAlias<"lb $rd, (${rs1})",  (LB  GPR:$rd, GPR:$rs1, 0, SS), 2>;
def : InstAlias<"lw $rd, (${rs1})",  (LW  GPR:$rd, GPR:$rs1, 0, SS), 2>;
def : InstAlias<"sb $rs2, (${rs1})", (SB  GPR:$rs2, GPR:$rs1, 0, SS), 2>;
def : InstAlias<"sw $rs2, (${rs1})", (SW  GPR:$rs2, GPR:$rs1, 0, SS), 2>;
}

def : MnemonicAlias<"bltu", "bc">;
def : MnemonicAlias<"bgeu", "bnc">;
def : MnemonicAlias<"beq", "bz">;
def : MnemonicAlias<"bne", "bnz">;
