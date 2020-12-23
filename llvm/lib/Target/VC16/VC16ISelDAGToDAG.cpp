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

  void PostprocessISelDAG() override;

  void Select(SDNode *Node) override;

  bool SelectInlineAsmMemoryOperand(const SDValue &Op, unsigned ConstraintID,
                                    std::vector<SDValue> &OutOps) override;

  bool SelectAddrFI(SDValue Addr, SDValue &Base);

// Include the pieces autogenerated from the target description.
#include "VC16GenDAGISel.inc"

private:
  void doPeepholeLoadStoreLEA();
};
} // namespace

void VC16DAGToDAGISel::PostprocessISelDAG() { doPeepholeLoadStoreLEA(); }

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
    ReplaceNode(Node, CurDAG->getMachineNode(VC16::LEA, DL, VT, TFI, Imm));
    return;
  }

  // Select the default instruction.
  SelectCode(Node);
}

bool VC16DAGToDAGISel::SelectInlineAsmMemoryOperand(
    const SDValue &Op, unsigned ConstraintID, std::vector<SDValue> &OutOps) {
  switch (ConstraintID) {
  case InlineAsm::Constraint_i:
  case InlineAsm::Constraint_m:
    // We just support simple memory operands that have a single address
    // operand and need no special handling.
    OutOps.push_back(Op);
    return false;
  default:
    break;
  }

  return true;
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

void VC16DAGToDAGISel::doPeepholeLoadStoreLEA() {
  SelectionDAG::allnodes_iterator Position(CurDAG->getRoot().getNode());
  ++Position;

  while (Position != CurDAG->allnodes_begin()) {
    SDNode *N = &*--Position;
    // Skip dead nodes and any non-machine opcodes.
    if (N->use_empty() || !N->isMachineOpcode())
      continue;

    int OffsetOpIdx;
    int BaseOpIdx;

    // Only attempt this optimisation for loads and stores.
    switch (N->getMachineOpcode()) {
    default:
      continue;
    case VC16::LB:
    case VC16::LW:
      BaseOpIdx = 0;
      OffsetOpIdx = 1;
      break;
    case VC16::SB:
    case VC16::SW:
      BaseOpIdx = 1;
      OffsetOpIdx = 2;
      break;
    }

    // Currently, the load/store offset must be 0 to be considered for this
    // peephole optimisation.
    if (!isa<ConstantSDNode>(N->getOperand(OffsetOpIdx)) ||
        N->getConstantOperandVal(OffsetOpIdx) != 0)
      continue;

    SDValue Base = N->getOperand(BaseOpIdx);
    // If the base is an LEA, we can merge it in to the load/store.
    if (!Base.isMachineOpcode() || Base.getMachineOpcode() != VC16::LEA)
      continue;

    SDValue BaseUpper = Base.getOperand(0);
    const bool isUpperLui = BaseUpper.isMachineOpcode() &&
                            BaseUpper.getMachineOpcode() == VC16::LUI;

    SDValue ImmUpperOperand;
    SDValue ImmOperand = Base.getOperand(1);
    if (auto Const = dyn_cast<ConstantSDNode>(ImmOperand)) {
      const int64_t ImmLowerValue = Const->getSExtValue();
      if (ImmLowerValue < 0) {
        if (!isUpperLui) {
          continue;
        }
        ImmUpperOperand = BaseUpper.getOperand(0);
        if (auto UpperConst = dyn_cast<ConstantSDNode>(ImmUpperOperand)) {
          const uint64_t ImmValue =
              (UpperConst->getZExtValue() << 5) + ImmLowerValue;

          ImmUpperOperand = CurDAG->getTargetConstant(
              (ImmValue >> 5) & 0x7ff, SDLoc(ImmUpperOperand),
              ImmUpperOperand.getValueType());
          ImmOperand = CurDAG->getTargetConstant(
              static_cast<uint64_t>(ImmLowerValue) & 0x1f, SDLoc(ImmOperand),
              ImmOperand.getValueType());
        } else {
          continue;
        }
      } else {
        ImmOperand = CurDAG->getTargetConstant(ImmLowerValue, SDLoc(ImmOperand),
                                               ImmOperand.getValueType());
      }
    } else if (auto GA = dyn_cast<GlobalAddressSDNode>(ImmOperand)) {
      if (!isUpperLui) {
        continue;
      }
      ImmUpperOperand = BaseUpper.getOperand(0);
      if (auto UpperGA = dyn_cast<GlobalAddressSDNode>(ImmUpperOperand)) {
        ImmUpperOperand = CurDAG->getTargetGlobalAddress(
            UpperGA->getGlobal(), SDLoc(ImmUpperOperand),
            ImmUpperOperand.getValueType(), UpperGA->getOffset(),
            VC16II::MO_HIU);
      } else {
        continue;
      }
      ImmOperand = CurDAG->getTargetGlobalAddress(
          GA->getGlobal(), SDLoc(ImmOperand), ImmOperand.getValueType(),
          GA->getOffset(), GA->getTargetFlags());
    } else {
      continue;
    }

    LLVM_DEBUG(dbgs() << "Folding add-immediate into mem-op:\nBase:    ");
    LLVM_DEBUG(Base->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\nBaseUpper:    ");
    LLVM_DEBUG(BaseUpper->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\nN: ");
    LLVM_DEBUG(N->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\nImmOperand: ");
    LLVM_DEBUG(ImmOperand->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\nImmUpperOperand: ");
    LLVM_DEBUG(ImmUpperOperand->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\n");

    SDValue NewBaseUpper = BaseUpper;
    if (ImmUpperOperand) {
      assert(isUpperLui && "Upper operand but not LUI.");
      NewBaseUpper = SDValue(CurDAG->getMachineNode(VC16::LUI, SDLoc(BaseUpper),
                                                    BaseUpper.getValueType(),
                                                    ImmUpperOperand),
                             0);
    }

    // Modify the offset operand of the load/store.
    if (BaseOpIdx == 0) // Load
      CurDAG->UpdateNodeOperands(N, NewBaseUpper, ImmOperand, N->getOperand(2));
    else // Store
      CurDAG->UpdateNodeOperands(N, N->getOperand(0), NewBaseUpper, ImmOperand,
                                 N->getOperand(3));

    // The LEA and LUI may now be dead, in which case remove it.
    if (Base.getNode()->use_empty())
      CurDAG->RemoveDeadNode(Base.getNode());
    if (BaseUpper.getNode()->use_empty())
      CurDAG->RemoveDeadNode(BaseUpper.getNode());
  }
}
