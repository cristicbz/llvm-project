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
  def X0: VC16Reg<0, "x0", ["sp"]>, DwarfRegNum<[0]>;
  def X1: VC16Reg<1, "x1", ["ra"]>, DwarfRegNum<[1]>;
  def X2: VC16Reg<2, "x2", ["s0", "fp"]>, DwarfRegNum<[2]>;
  def X3: VC16Reg<3, "x3", ["s1"]>, DwarfRegNum<[3]>;
  def X4: VC16Reg<4, "x4", ["a0"]>, DwarfRegNum<[4]>;
  def X5: VC16Reg<5, "x5", ["a1"]>, DwarfRegNum<[5]>;
  def X6: VC16Reg<6, "x6", ["a2"]>, DwarfRegNum<[6]>;
  def X7: VC16Reg<7, "x7", ["t0"]>, DwarfRegNum<[7]>;
}

// The order of registers represents the preferred allocation sequence.
// Registers are listed in the order caller-save, callee-save, specials.
def GPR : RegisterClass<"VC16", [i16], 16, (add
  X6, X5, X4, X7, X3, X2, X1, X0
)>;

def GPR_NoSP : RegisterClass<"VC16", [i16], 16, (add
  X6, X5, X4, X7, X3, X2, X1
)>;

// Status register.
def FLAGS : VC16Reg<4, "FLAGS", ["FLAGS"]>, DwarfRegNum<[8]>;
def CCR : RegisterClass<"VC16", [i16], 16, (add FLAGS)>
{
  let CopyCost = -1;      // Don't allow copying of status registers
}
