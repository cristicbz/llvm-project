class VCInst<dag outs, dag ins, string opcodestr, string argstr,
             list<dag> pattern>
    : Instruction {
  field bits<16> Inst;
  // SoftFail is a field the disassembler can use to provide a way for
  // instructions to not match without killing the whole decode process. It is
  // mainly used for ARM, but Tablegen expects this field to exist or it fails
  // to build the decode table.
  field bits<16> SoftFail = 0;
  let Size = 2;
  let Namespace = "VC";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = opcodestr # "\t" # argstr;
  let Pattern = pattern;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern>
    : VCInst<outs, ins, "", "", pattern> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

class VCInstRI11<bits<2> op2, dag outs, dag ins, string opcodestr,
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<11> imm11;
  bits<3> rd;

  let Inst{15-14} = imm11{10-9};
  let Inst{13-11} = rd;
  let Inst{10-2} = imm11{8-0};
  let Inst{1-0} = op2;
}

class VCInstM<bits<3> op3, dag outs, dag ins, string opcodestr,
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<5> imm5;
  bits<3> rd;
  bits<3> rs;

  // TODO(cristicbz): Add support for segment selectors, currently just point to
  // stack.
  // bits<2> ss;

  let Inst{15-14} = imm5{4-3};
  let Inst{13-11} = rd;
  let Inst{10-8} = rs;
  let Inst{7-6} = 0b11;
  let Inst{5-3} = imm5{2-0};
  let Inst{2-0} = op3;
}

class VCInstRI10<bits<3> op3, dag outs, dag ins, string opcodestr,
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<10> imm10;
  bits<3> rd;

  let Inst{15-14} = imm10{9-8};
  let Inst{13-11} = rd;
  let Inst{10-3} = imm10{7-0};
  let Inst{2-0} = op3;
}

class VCInstRRI5<bits<5> op5, dag outs, dag ins, string opcodestr,
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
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
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<8> imm8;

  let Inst{15-8} = imm8;
  let Inst{7-0} = op8;
}


class VCInstRR<bits<2> op2, bits<8> op8, dag outs, dag ins, string opcodestr,
    string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<4> shamt;
  bits<3> rs;
  bits<3> rd;

  let Inst{15-14} = op2;
  let Inst{13-11} = rd;
  let Inst{10-8} = rs;
  let Inst{7-0} = op8;
}

class VCInstRI4<bit arithshift, bits<8> op8, dag outs, dag ins,
    string opcodestr, string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<4> shamt;
  bits<3> rd;

  let Inst{15} = arithshift;
  let Inst{14} = shamt{3};
  let Inst{13-11} = rd;
  let Inst{10-8} = shamt{2-0};
  let Inst{7-0} = op8;
}


class VCInstRI5<bits<8> op8, dag outs, dag ins,
    string opcodestr, string argstr>
    : VCInst<outs, ins, opcodestr, argstr, []>
{
  bits<5> imm5;
  bits<3> rd;

  let Inst{15-14} = imm5{4-3};
  let Inst{13-11} = rd;
  let Inst{10-8} = imm5{2-0};
  let Inst{7-0} = op8;
}
