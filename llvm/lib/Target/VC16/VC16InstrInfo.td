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

def simm11_lsb0 : Operand<i16> {
  let ParserMatchClass = SImmAsmOperand<11, "Lsb0">;
  let EncoderMethod = "getImmOpValueAsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<11>";
}
def uimm11 : Operand<i16> {
  let ParserMatchClass = UImmAsmOperand<11>;
  let DecoderMethod = "decodeUImmOperand<11>";
}
def uimm4 : Operand<i16> {
  let ParserMatchClass = UImmAsmOperand<4>;
  let DecoderMethod = "decodeUImmOperand<4>";
}
def simm5 : Operand<i16> {
  let ParserMatchClass = SImmAsmOperand<5>;
  let DecoderMethod = "decodeSImmOperand<5>";
}
def uimm5 : Operand<i16> {
  let ParserMatchClass = UImmAsmOperand<5>;
  let DecoderMethod = "decodeUImmOperand<5>";
}
def uimm6_lsb0 : Operand<i16> {
  let ParserMatchClass = UImmAsmOperand<6, "Lsb0">;
  let EncoderMethod = "getImmOpValueLsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<6>";
}
def simm9_lsb0 : Operand<i16> {
  let ParserMatchClass = SImmAsmOperand<9, "Lsb0">;
  let EncoderMethod = "getImmOpValueAsr1";
  let DecoderMethod = "decodeSImmOperandAndLsl1<9>";
}

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_ri5<bits<8> op8, string opcodestr>
    : VCInstRI5<op8, (outs GPR:$rd), (ins simm5:$imm5), opcodestr, "$rd, $imm5">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_ri4<bit arithshift, bits<8> op8, string opcodestr>
    : VCInstRI4<arithshift, op8, (outs GPR:$rd), (ins uimm4:$imm4), opcodestr, "$rd, $imm4">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_rr<bits<2> op2, bits<8> op8, string opcodestr>
    : VCInstRR<op2, op8, (outs GPR:$rd), (ins GPR:$rs), opcodestr, "$rd, $rs">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class BranchCC_i<bits<8> op8, string opcodestr>
    : VCInstB<op8, (outs), (ins simm9_lsb0:$imm8), opcodestr, "$imm8"> {
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
def JALR : VCInstRRI5<0b00111, (outs GPR:$rd), (ins GPR:$rs1, uimm6_lsb0:$imm5),
                      "jalr", "$rd, $rs1, $imm5">;
} // hasSideEffects = 0, mayLoad = 0, mayStore = 0


let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
def LW : VCInstM<0b001, (outs GPR:$rd), (ins GPR:$rs1, uimm6_lsb0:$imm5),
                 "lw", "$rd, ${imm5}(${rs1})">;

def LB : VCInstM<0b101, (outs GPR:$rd), (ins GPR:$rs1, uimm5:$imm5),
                 "lb", "$rd, ${imm5}(${rs1})">;

} // hasSideEffects = 0, mayLoad = 1, mayStore = 0

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
def SW : VCInstM<0b010, (outs), (ins GPR:$rs2, GPR:$rs1, uimm6_lsb0:$imm5),
                 "sw", "$rs2, ${imm5}(${rs1})">;

def SB : VCInstM<0b110, (outs), (ins GPR:$rs2, GPR:$rs1, uimm5:$imm5),
                 "sb", "$rs2, ${imm5}(${rs1})">;
} // hasSideEffects = 0, mayLoad = 0, mayStore = 1


def   J  : BranchCC_i<0b00001111, "j">;
def  BZ  : BranchCC_i<0b00101111, "bz">;
def BNZ  : BranchCC_i<0b01001111, "bnz">;
def  BN  : BranchCC_i<0b01101111, "bn">;
def BNN  : BranchCC_i<0b10001111, "bnn">;
def BLT  : BranchCC_i<0b10101111, "blt">;
def BGE  : BranchCC_i<0b11001111, "bge">;
def BNC  : BranchCC_i<0b11101111, "bnc">;


def  ADD : ALU_rr <0b00, 0b00110111, "add">;
def  SLL : ALU_rr <0b00, 0b01110111, "sll">;
def  SUB : ALU_rr <0b00, 0b10110111, "sub">;
def  SRL : ALU_rr <0b00, 0b11110111, "srl">;

def  ADC : ALU_rr <0b01, 0b00110111, "adc">;
def  NEG : ALU_rr <0b01, 0b01110111, "neg">;
def  SBB : ALU_rr <0b01, 0b10110111, "sbb">;
// def   SX : ALU_rr <0b01, 0b11110111, "sx">;

def  AND : ALU_rr <0b10, 0b00110111, "and">;
def  XOR : ALU_rr <0b10, 0b01110111, "xor">;
def   OR : ALU_rr <0b10, 0b10110111, "or">;
def  NOT : ALU_rr <0b10, 0b11110111, "not">;

def TEST : ALU_rr <0b11, 0b00110111, "test">;
def   MV : ALU_rr <0b11, 0b01110111, "mv">;
def  CMP : ALU_rr <0b11, 0b10110111, "cmp">;
def  SRA : ALU_rr <0b11, 0b11110111, "sra">;


def SLLI : ALU_ri4<0b0,  0b00011111, "slli">;
def SRLI : ALU_ri4<0b0,  0b10011111, "srli">;
def SRAI : ALU_ri4<0b1,  0b10011111, "srai">;

def ADDI : ALU_ri5<      0b01011111, "addi">;
def ADCI : ALU_ri5<      0b11011111, "adci">;
def  ORI : ALU_ri5<      0b00111111, "ori">;
def XORI : ALU_ri5<      0b01111111, "xori">;
def  LLI : ALU_ri5<      0b10111111, "lli">;
def ANDI : ALU_ri5<      0b11111111, "andi">;

//===----------------------------------------------------------------------===//
// Pseudo-nstructions
//===----------------------------------------------------------------------===//
def : InstAlias<"nop",                     (ANDI      R7,      -1)>;
def : InstAlias<"jalr ${rs}",              (JALR      R6, GPR:$rs,       0), 3>;
def : InstAlias<"jalr ${offset}(${rs})",   (JALR      R6, GPR:$rs, uimm6_lsb0:$offset)>;
def : InstAlias<"jalr ${rd}, ${rs}",       (JALR GPR:$rd, GPR:$rs,       0), 2>;
def : InstAlias<"ret",                     (JALR      R6,      R6,       0), 4>;
def : InstAlias<"bltu ${offset}",          (BN        simm9_lsb0:$offset)>;
def : InstAlias<"bgeu ${offset}",          (BNN       simm9_lsb0:$offset)>;
def : InstAlias<"beq  ${offset}",          (BZ        simm9_lsb0:$offset)>;
def : InstAlias<"bne  ${offset}",          (BNZ       simm9_lsb0:$offset)>;


let EmitPriority = 0 in {
def : InstAlias<"lb $rd, (${rs1})",  (LB  GPR:$rd, GPR:$rs1, 0)>;
def : InstAlias<"lw $rd, (${rs1})",  (LW  GPR:$rd, GPR:$rs1, 0)>;
def : InstAlias<"sb $rs2, (${rs1})", (SB  GPR:$rs2, GPR:$rs1, 0)>;
def : InstAlias<"sw $rs2, (${rs1})", (SW  GPR:$rs2, GPR:$rs1, 0)>;

def : InstAlias<"add $rd, $imm5",  (ADDI  GPR:$rd, simm5:$imm5)>;
def : InstAlias<"and $rd, $imm5",  (ANDI  GPR:$rd, simm5:$imm5)>;
def : InstAlias<"xor $rd, $imm5",  (XORI  GPR:$rd, simm5:$imm5)>;
def : InstAlias<"or $rd, $imm5",   (ORI   GPR:$rd, simm5:$imm5)>;
def : InstAlias<"sll $rd, $shamt", (SLLI  GPR:$rd, uimm4:$shamt)>;
def : InstAlias<"srl $rd, $shamt", (SRLI  GPR:$rd, uimm4:$shamt)>;
def : InstAlias<"sra $rd, $shamt", (SRAI  GPR:$rd, uimm4:$shamt)>;
}
