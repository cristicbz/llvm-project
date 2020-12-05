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
def simm10 : Operand<i16>;
def uimm11 : Operand<i16>;
def uimm4 : Operand<i16>;
def simm5 : Operand<i16>;
def uimm5 : Operand<i16>;
def uimm6 : Operand<i16>;
def imm8 : Operand<i16>;

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

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//


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
def ANDI : ALU_ri5<      0b11111111, "and">;
