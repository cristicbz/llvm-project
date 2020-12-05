//===-- VC16RegisterInfo.td - VC16 Register defs -----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the VC16 register files
//===----------------------------------------------------------------------===//

let Namespace = "VC16" in {
class VC16Reg<bits<3> Enc, string n, list<string> alt = []>: Register<n> {
  let HWEncoding{2-0} = Enc;
  let AltNames = alt;
}

def ABIRegAltName : RegAltNameIndex;
} // Namespace = "VC16"


let RegAltNameIndices = [ABIRegAltName] in {
  def R0: VC16Reg<0, "r0", []>, DwarfRegNum<[0]>;
  def R1: VC16Reg<1, "r1", []>, DwarfRegNum<[1]>;
  def R2: VC16Reg<2, "r2", []>, DwarfRegNum<[2]>;
  def R3: VC16Reg<3, "r3", []>, DwarfRegNum<[3]>;
  def R4: VC16Reg<4, "r4", []>, DwarfRegNum<[4]>;
  def R5: VC16Reg<5, "r5", []>, DwarfRegNum<[5]>;
  def R6: VC16Reg<6, "r6", ["ra"]>, DwarfRegNum<[6]>;
  def R7: VC16Reg<7, "r7", ["sp"]>, DwarfRegNum<[7]>;
}

// TODO(cristicbz): Order in order of how they should be allocated.
def GPR : RegisterClass<"VC16", [i16], 16, (add
  (sequence "R%u", 0, 7)
)>;

// Status register.
def SREG : VC16Reg<8, "FLAGS">, DwarfRegNum<[88]>;
def CCR : RegisterClass<"VC16", [i8], 8, (add SREG)>
{
  let CopyCost = -1;      // Don't allow copying of status registers
}

