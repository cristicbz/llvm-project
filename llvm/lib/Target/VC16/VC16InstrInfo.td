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
def SDT_VC16SelectCC     : SDTypeProfile<1, 5, [SDTCisSameAs<1, 2>,
                                                SDTCisSameAs<0, 4>,
                                                SDTCisSameAs<4, 5>]>;


def Call         : SDNode<"VC16ISD::CALL", SDT_VC16Call,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                           SDNPVariadic]>;
def CallSeqStart : SDNode<"ISD::CALLSEQ_START", SDT_VC16CallSeqStart,
                          [SDNPHasChain, SDNPOutGlue]>;
def CallSeqEnd   : SDNode<"ISD::CALLSEQ_END", SDT_VC16CallSeqEnd,
                          [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def RetFlag      : SDNode<"VC16ISD::RET_FLAG", SDTNone,
                          [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def SelectCC     : SDNode<"VC16ISD::SELECT_CC", SDT_VC16SelectCC,
                          [SDNPInGlue]>;

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
  let DecoderMethod = "decodeUImmOperand<11>";
}
def uimm4 : Operand<i16>, ImmLeaf<i16, [{return isUInt<4>(Imm);}]>  {
  let ParserMatchClass = UImmAsmOperand<4>;
  let DecoderMethod = "decodeUImmOperand<4>";
}
def simm5 : Operand<i16>, ImmLeaf<i16, [{return isInt<5>(Imm);}]>  {
  let ParserMatchClass = SImmAsmOperand<5>;
  let DecoderMethod = "decodeSImmOperand<5>";
}
def uimm5 : Operand<i16>, ImmLeaf<i16, [{return isUInt<5>(Imm);}]>  {
  let ParserMatchClass = UImmAsmOperand<5>;
  let DecoderMethod = "decodeUImmOperand<5>";
}
def uimm6_lsb0 : Operand<i16>, ImmLeaf<i16, [{return isShiftedUInt<5, 1>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<6, "Lsb0">;
  let EncoderMethod = "getImmOpValueLsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<6>";
}
def simm9_lsb0 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<9, "Lsb0">;
  let EncoderMethod = "getImmOpValueAsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<9>";
}

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_BinaryCC_ri5<bits<8> op8, string opcodestr>
    : VCInstRI5<op8, (outs GPR:$rd_wb), (ins GPR:$rd, simm5:$imm5),
                opcodestr, "$rd, $imm5"> {
  let Constraints = "$rd = $rd_wb";
  let Defs = [FR];
}

class ALU_Unary_ri5<bits<8> op8, string opcodestr>
    : VCInstRI5<op8, (outs GPR:$rd), (ins simm5:$imm5),
                opcodestr, "$rd, $imm5">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_ri4<bit arithshift, bits<8> op8, string opcodestr>
    : VCInstRI4<arithshift, op8, (outs GPR:$rd_wb), (ins GPR:$rd, uimm4:$shamt),
                opcodestr, "$rd, $shamt"> {
  let Constraints = "$rd = $rd_wb";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Unary_rr<bits<2> op2, bits<8> op8, string opcodestr>
    : VCInstRR<op2, op8, (outs GPR:$rd), (ins GPR:$rs),
               opcodestr, "$rd, $rs">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_Binary_rr<bits<2> op2, bits<8> op8, string opcodestr>
    : VCInstRR<op2, op8, (outs GPR:$rd_wb), (ins GPR:$rd, GPR:$rs),
               opcodestr, "$rd, $rs"> {
  let Constraints = "$rd = $rd_wb";
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_BinaryCC_rr<bits<2> op2, bits<8> op8, string opcodestr>
    : VCInstRR<op2, op8, (outs GPR:$rd_wb), (ins GPR:$rd, GPR:$rs),
               opcodestr, "$rd, $rs"> {
  let Constraints = "$rd = $rd_wb";
  let Defs = [FR];
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_SetCC_rr<bits<2> op2, bits<8> op8, string opcodestr>
    : VCInstRR<op2, op8, (outs CCR:$fr), (ins GPR:$rs1, GPR:$rs2),
               opcodestr, "$rs1, $rs2">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class BranchCC_i<bits<8> op8, string opcodestr>
    : VCInstB<op8, (outs), (ins CCR:$fr, simm9_lsb0:$imm8), opcodestr, "$imm8"> {
    let isBranch = 1;
    let isTerminator = 1;
}

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def LUI  : VCInstRI11<0b00, (outs GPR:$rd), (ins uimm11:$imm11),
                      "lui", "$rd, $imm11">;

let isCall = 1 in
def JAL  : VCInstRI10<0b011, (outs GPR:$rd), (ins simm11_lsb0:$imm10),
                      "jal", "$rd, $imm10">;

let isCall = 1 in
def JALR : VCInstRRI5<0b00111, (outs GPR:$rd), (ins GPR:$rs, uimm6_lsb0:$imm5),
                      "jalr", "$rd, $rs, $imm5">;
} // hasSideEffects = 0, mayLoad = 0, mayStore = 0


let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
def LW : VCInstM<0b001, (outs GPR:$rs2), (ins GPR:$rs1, uimm6_lsb0:$imm5),
                 "lw", "$rs2, ${imm5}(${rs1})">;

def LB : VCInstM<0b101, (outs GPR:$rs2), (ins GPR:$rs1, uimm5:$imm5),
                 "lb", "$rs2, ${imm5}(${rs1})">;

} // hasSideEffects = 0, mayLoad = 1, mayStore = 0

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
def SW : VCInstM<0b010, (outs), (ins GPR:$rs2, GPR:$rs1, uimm6_lsb0:$imm5),
                 "sw", "$rs2, ${imm5}(${rs1})">;

def SB : VCInstM<0b110, (outs), (ins GPR:$rs2, GPR:$rs1, uimm5:$imm5),
                 "sb", "$rs2, ${imm5}(${rs1})">;
} // hasSideEffects = 0, mayLoad = 0, mayStore = 1

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
def J  : VCInstB<0b00001111, (outs), (ins simm9_lsb0:$imm8), "j", "$imm8"> {
    let isBranch = 1;
    let isTerminator = 1;
    let isBarrier = 1;
}

def  BZ  : BranchCC_i<0b00101111, "bz">;
def BNZ  : BranchCC_i<0b01001111, "bnz">;
def  BN  : BranchCC_i<0b01101111, "bn">;
def BNN  : BranchCC_i<0b10001111, "bnn">;
def BLT  : BranchCC_i<0b10101111, "blt">;
def BGE  : BranchCC_i<0b11001111, "bge">;
def BNC  : BranchCC_i<0b11101111, "bnc">;


def  ADD : ALU_BinaryCC_rr  <0b00, 0b00110111, "add">;
def  SLL : ALU_Binary_rr    <0b00, 0b01110111, "sll">;
def  SUB : ALU_BinaryCC_rr  <0b00, 0b10110111, "sub">;
def  SRL : ALU_Binary_rr    <0b00, 0b11110111, "srl">;

def  ADC : ALU_BinaryCC_rr  <0b01, 0b00110111, "adc">;
def  NEG : ALU_Unary_rr     <0b01, 0b01110111, "neg">;
def  SBB : ALU_BinaryCC_rr  <0b01, 0b10110111, "sbb">;

def  AND : ALU_BinaryCC_rr  <0b10, 0b00110111, "and">;
def  XOR : ALU_BinaryCC_rr  <0b10, 0b01110111, "xor">;
def   OR : ALU_BinaryCC_rr  <0b10, 0b10110111, "or">;
def  NOT : ALU_Unary_rr     <0b10, 0b11110111, "not">;

def TEST : ALU_SetCC_rr     <0b11, 0b00110111, "test">;
def  CMP : ALU_SetCC_rr     <0b11, 0b10110111, "cmp">;
def   MV : ALU_Unary_rr     <0b11, 0b01110111, "mv">;
def  SRA : ALU_Binary_rr    <0b11, 0b11110111, "sra">;

def SLLI : ALU_ri4          <0b0,  0b00011111, "slli">;
def SRLI : ALU_ri4          <0b0,  0b10011111, "srli">;
def SRAI : ALU_ri4          <0b1,  0b10011111, "srai">;

def ADDI : ALU_BinaryCC_ri5 <      0b01011111, "addi">;
def ADCI : ALU_BinaryCC_ri5 <      0b11011111, "adci">;
def  ORI : ALU_BinaryCC_ri5 <      0b00111111, "ori">;
def XORI : ALU_BinaryCC_ri5 <      0b01111111, "xori">;
def ANDI : ALU_BinaryCC_ri5 <      0b11111111, "andi">;
def  LLI : ALU_Unary_ri5    <      0b10111111, "lli">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
def CHK : VCInstChk<0b01011111, (outs CCR:$fr), (ins GPR:$rs), "addi", "$rs, 0">;

//===----------------------------------------------------------------------===//
// Pseudo-instructions and codegen patterns
//
// Naming convention: For 'generic' pattern classes, we use the naming
// convention PatTy1Ty2. For pattern classes which offer a more complex
// expension, prefix the class name, e.g. BccPat.
//===----------------------------------------------------------------------===//

/// Generic pattern classes

class PatGprGpr<SDPatternOperator OpNode, VCInstRR Inst>
    : Pat<(OpNode GPR:$rd, GPR:$rs), (Inst GPR:$rd, GPR:$rs)>;
class PatGprSimm5<SDPatternOperator OpNode, VCInstRI5 Inst>
    : Pat<(OpNode GPR:$rs, simm5:$imm5), (Inst GPR:$rs, simm5:$imm5)>;
class PatGprUimm4<SDPatternOperator OpNode, VCInstRI4 Inst>
    : Pat<(OpNode GPR:$rs, uimm4:$shamt), (Inst GPR:$rs, uimm4:$shamt)>;

// def : PatGprGpr<addc,TEST : ALU_Binary_rr <0b11, 0b00110111, "test">;


// ALU binary
def : PatGprGpr<add, ADD>;
def : PatGprGpr<shl, SLL>;
def : PatGprGpr<sub, SUB>;
def : PatGprGpr<srl, SRL>;
def : PatGprGpr<adde, ADC>;
def : PatGprGpr<sube, SBB>;
def : PatGprGpr<and, AND>;
def : PatGprGpr<xor, XOR>;
def : PatGprGpr<or,  OR>;
def : PatGprGpr<sra, SRA>;

// ALU unary
def : Pat<(ineg GPR:$rs), (NEG GPR:$rs)>;
def : Pat<(not GPR:$rs), (NOT GPR:$rs)>;

// ALU immediate
def : PatGprSimm5<or, ORI>;
def : PatGprSimm5<add, ADDI>;
def : PatGprSimm5<adde, ADCI>;
def : PatGprSimm5<and, ANDI>;
def : PatGprSimm5<xor, XORI>;
def : PatGprUimm4<shl, SLLI>;
def : PatGprUimm4<srl, SRLI>;
def : PatGprUimm4<sra, SRAI>;

// Materialize immediates
def simm16 : ImmLeaf<i16, [{return isInt<16>(Imm);}]>;
def simm16_shift5 : ImmLeaf<i16, [{return isShiftedInt<11, 5>(Imm);}]>;
def uimm16_shift5 : ImmLeaf<i16, [{return isShiftedUInt<11, 5>(Imm);}]>;

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

def : Pat<(simm5:$imm), (LLI simm5:$imm)>;
def : Pat<(simm16_shift5:$imm), (LUI (HI20U $imm))>;
def : Pat<(simm16:$imm), (ADDI (LUI (HI20S imm:$imm)), (LO5Sext imm:$imm))>;

let usesCustomInserter = 1 in
def Select_GPR_Using_CC_GPR
    : Pseudo<(outs GPR:$dst),
             (ins GPR:$lhs, GPR:$rhs, i16imm:$imm, GPR:$src, GPR:$src2),
             [(set i16:$dst, (SelectCC GPR:$lhs, GPR:$rhs,
              (i16 imm:$imm), GPR:$src, GPR:$src2))]>;


/// Branches and jumps
// Match `(brcond (CondOp ..), ..)` and lower to the appropriate CMP + branch
// sequence.
class BccPat<PatFrag CondOp, VCInstB Inst>
    : Pat<(brcond (i16 (CondOp GPR:$rs1, GPR:$rs2)), bb:$imm8),
          (Inst (CMP GPR:$rs1, GPR:$rs2), simm9_lsb0:$imm8)>;

def : BccPat<seteq, BZ>;
def : BccPat<setne, BNZ>;
def : BccPat<setlt, BLT>;
def : BccPat<setge, BGE>;
def : BccPat<setult, BN>;
def : BccPat<setuge, BNN>;

class BccSwapPat<PatFrag CondOp, VCInstB InstBcc>
    : Pat<(brcond (i16 (CondOp GPR:$rs1, GPR:$rs2)), bb:$imm8),
          (InstBcc (CMP GPR:$rs2, GPR:$rs1), bb:$imm8)>;

// Condition codes that don't have matching RISC-V branch instructions, but
// are trivially supported by swapping the two input operands
def : BccSwapPat<setgt, BLT>;
def : BccSwapPat<setle, BGE>;
def : BccSwapPat<setugt, BN>;
def : BccSwapPat<setule, BNN>;

// An extra pattern is needed for a brcond without a setcc (i.e. where the
// condition was calculated elsewhere).
def : Pat<(brcond GPR:$cond, bb:$imm8),
          (BNZ (CHK GPR:$cond), bb:$imm8)>;

def : Pat<(br bb:$imm8), (J bb:$imm8)>;

let isBarrier = 1, isBranch = 1, isTerminator = 1, mayStore = 0, mayLoad = 0, hasSideEffects = 0, Defs=[R5] in
def PseudoBR : Pseudo<(outs), (ins simm11_lsb0:$imm10), [(br bb:$imm10)]>,
               PseudoInstExpansion<(JAL R5, simm11_lsb0:$imm10)>;

let isCall = 1, Defs=[R6] in
def PseudoCALL : Pseudo<(outs), (ins GPR:$rs1), [(Call GPR:$rs1)]>,
                 PseudoInstExpansion<(JALR R6, GPR:$rs1, 0)>;

let isBarrier = 1, isReturn = 1, isTerminator = 1, Defs=[R5], Uses=[R6] in
def PseudoRET : Pseudo<(outs), (ins), [(RetFlag)]>,
                PseudoInstExpansion<(JALR R5, R6, 0)>;

/// Loads
multiclass LdPat<PatFrag LoadOp, ImmLeaf LoImmT, VCInst Inst> {
  def : Pat<(LoadOp GPR:$rs1), (Inst GPR:$rs1, 0)>;
  def : Pat<(LoadOp (add GPR:$rs1, LoImmT:$imm5)), (Inst GPR:$rs1, LoImmT:$imm5)>;
}

def : Pat<(sextloadi8 GPR:$rs1), (SRAI (SLLI (LB GPR:$rs1, 0), 8), 8)>;
def : Pat<(sextloadi8 (add GPR:$rs1, uimm5:$imm5)),
          (SRAI (SLLI (LB GPR:$rs1, uimm5:$imm5), 8), 8)>;

defm : LdPat<extloadi8, uimm5, LB>;
defm : LdPat<zextloadi8, uimm5, LB>;
defm : LdPat<load, uimm6_lsb0, LW>;

/// Stores
multiclass StPat<PatFrag StoreOp, VCInst Inst> {
  def : Pat<(StoreOp GPR:$rs2, GPR:$rs1), (Inst GPR:$rs2, GPR:$rs1, 0)>;
  def : Pat<(StoreOp GPR:$rs2, (add GPR:$rs1, uimm5:$imm5)),
            (Inst GPR:$rs2, GPR:$rs1, uimm5:$imm5)>;
}

defm : StPat<truncstorei8, SB>;
defm : StPat<store, SW>;

/// Other pseudo-instructions

// Pessimistically assume the stack pointer will be clobbered
let Defs = [R7], Uses = [R7] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                              [(CallSeqStart timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP   : Pseudo<(outs), (ins i16imm:$amt1, i16imm:$amt2),
                              [(CallSeqEnd timm:$amt1, timm:$amt2)]>;
} // Defs = [R7], Uses = [R7]

/// Aliases
def : InstAlias<"nop",                     (ANDI      R7,      -1)>;
def : InstAlias<"jalr ${rs}",              (JALR      R6, GPR:$rs,       0), 3>;
def : InstAlias<"jalr ${offset}(${rs})",   (JALR      R6, GPR:$rs, uimm6_lsb0:$offset)>;
def : InstAlias<"jalr ${rd}, ${rs}",       (JALR GPR:$rd, GPR:$rs,       0), 2>;
def : InstAlias<"ret",                     (JALR      R5,      R6,       0), 4>;


let EmitPriority = 0 in {
def : InstAlias<"lb $rd, (${rs1})",  (LB  GPR:$rd, GPR:$rs1, 0)>;
def : InstAlias<"lw $rd, (${rs1})",  (LW  GPR:$rd, GPR:$rs1, 0)>;
def : InstAlias<"sb $rs2, (${rs1})", (SB  GPR:$rs2, GPR:$rs1, 0)>;
def : InstAlias<"sw $rs2, (${rs1})", (SW  GPR:$rs2, GPR:$rs1, 0)>;

// TODO(cristicbz): getMachineOpValue() needs to be adjusted for this to work.
// def : InstAlias<"add $rd, $imm5",  (ADDI  GPR:$rd, simm5:$imm5)>;
// def : InstAlias<"and $rd, $imm5",  (ANDI  GPR:$rd, simm5:$imm5)>;
// def : InstAlias<"xor $rd, $imm5",  (XORI  GPR:$rd, simm5:$imm5)>;
// def : InstAlias<"or $rd, $imm5",   (ORI   GPR:$rd, simm5:$imm5)>;
// def : InstAlias<"sll $rd, $shamt", (SLLI  GPR:$rd, uimm4:$shamt)>;
// def : InstAlias<"srl $rd, $shamt", (SRLI  GPR:$rd, uimm4:$shamt)>;
// def : InstAlias<"sra $rd, $shamt", (SRAI  GPR:$rd, uimm4:$shamt)>;
}

def : MnemonicAlias<"bltu", "bn">;
def : MnemonicAlias<"bgeu", "bnn">;
def : MnemonicAlias<"beq", "bz">;
def : MnemonicAlias<"bne", "bnz">;
