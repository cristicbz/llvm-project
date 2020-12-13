//===-- VC16ISelDAGToDAG.cpp - A dag to dag inst selector for VC16 --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the VC16 target.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/VC16MCTargetDesc.h"
#include "VC16.h"
#include "VC16TargetMachine.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "vc16-isel"

// VC16-specific code to select VC16 machine instructions for
// SelectionDAG operations.
namespace {
class VC16DAGToDAGISel final : public SelectionDAGISel {
public:
  explicit VC16DAGToDAGISel(VC16TargetMachine &TargetMachine)
      : SelectionDAGISel(TargetMachine) {}

  StringRef getPassName() const override {
    return "VC16 DAG->DAG Pattern Instruction Selection";
  }

  void Select(SDNode *Node) override;

  bool SelectAddrFI(SDValue Addr, SDValue &Base);

// Include the pieces autogenerated from the target description.
#include "VC16GenDAGISel.inc"
};
} // namespace

void VC16DAGToDAGISel::Select(SDNode *Node) {
  unsigned Opcode = Node->getOpcode();

  // If we have a custom node, we have already selected
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(dbgs() << "== "; Node->dump(CurDAG); dbgs() << "\n");
    Node->setNodeId(-1);
    return;
  } else {
    LLVM_DEBUG(dbgs() << "non-mc "; Node->dump(CurDAG); dbgs() << "\n");
  }

  if (Opcode == ISD::FrameIndex) {
    SDLoc DL(Node);
    SDValue Imm = CurDAG->getTargetConstant(0, DL, MVT::i16);
    int FI = dyn_cast<FrameIndexSDNode>(Node)->getIndex();
    EVT VT = Node->getValueType(0);
    SDValue TFI = CurDAG->getTargetFrameIndex(FI, VT);
    ReplaceNode(Node, CurDAG->getMachineNode(VC16::ADDI, DL, VT, TFI, Imm));
    return;
  }

  // Select the default instruction.
  SelectCode(Node);
}

bool VC16DAGToDAGISel::SelectAddrFI(SDValue Addr, SDValue &Base) {
  if (auto FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
    Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), MVT::i16);
    return true;
  }
  return false;
}

// This pass converts a legalized DAG into a VC16-specific DAG, ready
// for instruction scheduling.
FunctionPass *llvm::createVC16ISelDag(VC16TargetMachine &TM) {
  return new VC16DAGToDAGISel(TM);
}
