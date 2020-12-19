// Format specifies the encoding used by the instruction. This is used by
// VC16MCCodeEmitter to determine which form of fixup to use. These
// definitions must be kept in-sync with VC16BaseInfo.h.
class InstFormat<bits<4> val> {
  bits<4> Value = val;
}

def InstFormatPseudo : InstFormat<0>;
def InstFormatRI11   : InstFormat<1>;
def InstFormatM      : InstFormat<2>;
def InstFormatRI10   : InstFormat<3>;
def InstFormatRRI5   : InstFormat<4>;
def InstFormatB      : InstFormat<5>;

def InstFormatRR     : InstFormat<6>;
def InstFormatRI4    : InstFormat<7>;
def InstFormatRI5    : InstFormat<8>;

def InstFormatSI6    : InstFormat<9>;
def InstFormatCR     : InstFormat<10>;

class VCInst<dag outs, dag ins, string opcodestr, string argstr,
             list<dag> pattern, InstFormat format>
    : Instruction {
  field bits<16> Inst;
  // SoftFail is a field the disassembler can use to provide a way for
  // instructions to not match without killing the whole decode process. It is
  // mainly used for ARM, but Tablegen expects this field to exist or it fails
  // to build the decode table.
  field bits<16> SoftFail = 0;
  let Size = 2;
  let Namespace = "VC16";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = opcodestr # "\t" # argstr;
  let Pattern = pattern;

  let TSFlags{3-0} = format.Value;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern>
    : VCInst<outs, ins, "", "", pattern, InstFormatPseudo> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

class VCInstRI11<bits<2> op2, dag outs, dag ins, string opcodestr,
    string argstr, list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatRI11>
{
  bits<11> imm11;
  bits<3> rd;

  let Inst{15-14} = imm11{10-9};
  let Inst{13-11} = rd;
  let Inst{10-2} = imm11{8-0};
  let Inst{1-0} = op2;
}

class VCInstM<bits<3> op3, dag outs, dag ins, string opcodestr,
    string argstr, list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatM>
{
  bits<5> imm5;
  bits<3> rs2;
  bits<3> rs1;

  // TODO(cristicbz): Add support for segment selectors, currently just point to
  // stack.
  // bits<2> ss;

  let Inst{15-14} = imm5{4-3};
  let Inst{13-11} = rs2;
  let Inst{10-8} = rs1;
  let Inst{7-6} = 0b11;
  let Inst{5-3} = imm5{2-0};
  let Inst{2-0} = op3;
}

class VCInstRI10<bits<3> op3, dag outs, dag ins, string opcodestr,
    string argstr, list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatRI10>
{
  bits<10> imm10;
  bits<3> rd;

  let Inst{15-14} = imm10{9-8};
  let Inst{13-11} = rd;
  let Inst{10-3} = imm10{7-0};
  let Inst{2-0} = op3;
}

class VCInstRRI5<bits<5> op5, dag outs, dag ins, string opcodestr,
    string argstr, list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatRRI5>
{
  bits<5> imm5;
  bits<3> rd;
  bits<3> rs;

  let Inst{15-14} = imm5{4-3};
  let Inst{13-11} = rd;
  let Inst{10-8} = rs;
  let Inst{7-5} = imm5{2-0};
  let Inst{4-0} = op5;
}

class VCInstB<bits<8> op8, dag outs, dag ins, string opcodestr,
    string argstr, list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatB>
{
  bits<8> imm8;

  let Inst{15-8} = imm8;
  let Inst{7-0} = op8;
}


class VCInstRR<bits<2> op2, bits<8> op8, dag outs, dag ins, string opcodestr,
    list<dag> pattern=[]>
    : VCInst<outs, ins, opcodestr, "$r1, $r2", pattern, InstFormatRR>
{
  bits<3> r1;
  bits<3> r2;

  let Inst{15-14} = op2;
  let Inst{13-11} = r1;
  let Inst{10-8} = r2;
  let Inst{7-0} = op8;
}

class VCInstRI4<bit op1, bits<8> op8, dag outs, dag ins,
    string opcodestr, string argstr, list<dag> pattern = []>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatRI4>
{
  bits<4> imm4;
  bits<3> rs;

  let Inst{15} = op1;
  let Inst{14} = imm4{3};
  let Inst{13-11} = rs;
  let Inst{10-8} = imm4{2-0};
  let Inst{7-0} = op8;
}


class VCInstRI5<bits<8> op8, dag outs, dag ins,
    string opcodestr, string argstr, list<dag> pattern = []>
    : VCInst<outs, ins, opcodestr, argstr, pattern, InstFormatRI5>
{
  bits<5> imm5;
  bits<3> rs;

  let Inst{15-14} = imm5{4-3};
  let Inst{13-11} = rs;
  let Inst{10-8} = imm5{2-0};
  let Inst{7-0} = op8;
}
