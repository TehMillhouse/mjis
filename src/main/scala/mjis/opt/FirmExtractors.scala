package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtensions._

import scala.collection.JavaConversions._

object FirmExtractors {

  object NodeExtr {
    def unapplySeq(node: Node): Option[Seq[Node]] = Some(node.getPreds.toList)
  }

  object ConstExtr {
    def unapply(node: Node): Option[Int] = node match {
      case c: Const => Some(c.getTarval.asInt())
      case _ => None
    }
  }

  object ConvExtr {
    def unapply(node: Node): Option[Node] = node match {
      case c: Conv => Some(c.getOp)
      case _ => None
    }
  }

  object GenConvExtr {
    def unapply(node: Node): Option[Node] = node match {
      case ConvExtr(n) => Some(n)
      case _ => Some(node)
    }
  }

  object ProjExtr {
    def unapply(node: Node): Option[(Node, Int)] = node match {
      case proj: Proj => Some((proj.getPred, proj.getNum))
      case _ => None
    }
  }

  object AddExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case add: Add => Some((add.getLeft, add.getRight))
      case _ => None
    }
  }

  object AndExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case and: And => Some((and.getLeft, and.getRight))
      case _ => None
    }
  }

  object CondExtr {
    def unapply(node: Node): Option[Node] = node match {
      case cond: Cond => Some(cond.getSelector)
      case _ => None
    }
  }

  object MinusExtr {
    def unapply(node: Node): Option[Node] = node match {
      case minus: Minus => Some(minus.getOp)
      case _ => None
    }
  }
  object SubExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case sub: Sub => Some((sub.getLeft, sub.getRight))
      case _ => None
    }
  }

  object GenAddExtr {
    def unapply(node: Node): Option[(Option[Node], Int)] = node match {
      case ConstExtr(c) => Some((None, c))
      case AddExtr(n, ConstExtr(c)) => Some((Some(n), c))
      case _ => Some((Some(node), 0))
    }
  }

  object GenMulExtr {
    def unapply(node: Node): Option[(Option[Node], Int)] = node match {
      case MulExtr(n, ConstExtr(c)) => Some((Some(n), c))
      case _ => Some((Some(node), 1))
    }
  }

  object MulExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case mul: Mul => Some((mul.getLeft, mul.getRight))
      case _ => None
    }
  }

  object CmpExtr {
    def unapply(node: Node): Option[(Relation, Node, Node)] = node match {
      case cmp: Cmp => Some((cmp.getRelation, cmp.getLeft, cmp.getRight))
      case _ => None
    }
  }

  object DivExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case div: Div => Some((div.getLeft, div.getRight))
      case _ => None
    }
  }

  object ModExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case mod: Mod => Some((mod.getLeft, mod.getRight))
      case _ => None
    }
  }

  object MemberExtr {
    def unapply(node: Node): Option[(Node, Entity)] = node match {
      case member: Member => Some((member.getPtr, member.getEntity))
      case _ => None
    }
  }

  object SelExtr {
    /** Some(ptr, idx) or None */
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case sel: Sel => Some((sel.getPtr, sel.getIndex))
      case _ => None
    }
  }

  object AddressExtr {
    def unapply(address: Address): Option[Entity] = Some(address.getEntity)
  }

  object PhiExtr {
    def unapplySeq(node: Node): Option[Seq[Node]] = node match {
      case phi: Phi => Some(phi.getPreds.toList)
      case _ => None
    }
  }

  object ReturnExtr {
    def unapply(node: Node): Option[Option[Node]] = node match {
      case ret: Return => Some(
        // >= 2 because the Mem predecessor always exists
        if (ret.getPredCount >= 2)  Some(ret.getPred(1)) else None
      )
      case _ => None
    }
  }

  object CallExtr {
    /** Some(target, parameters) or None */
    def unapply(node: Node): Option[(Address, Seq[Node])] = node match {
      case call: Call => Some((call.getPtr.asInstanceOf[Address], call.getPreds.toList.drop(2) /* Mem and address */))
      case _ => None
    }
  }

  object ReturnOfCallExtr {
    /** Some(target, parameters) or None */
    def unapply(node: Node): Option[(Address, Seq[Node])] = node match {
      case ProjExtr(
        ProjExtr(
          CallExtr(target, parameters),
          Call.pnTResult
        ),
        0
      ) => Some((target, parameters))
      case _ => None
    }
  }

  object LoadExtr {
    def unapply(node: Node): Option[Node] = node match {
      case load: Load => Some(load.getPtr)
      case _ => None
    }
  }

  object MuxExtr {
    def unapply(node: Node): Option[(Node, Node, Node)] = node match {
      case mux: Mux => Some((mux.getSel, mux.getFalse, mux.getTrue))
      case _ => None
    }
  }

  object TargetValueExtr {
    def unapply(tarval: TargetValue): Option[Long] = {
      if (tarval.isConstant)
        Some(tarval.asLong())
      else
        None
    }
  }

  /* Detects simple if-then-else diamond structures in the graph. */
  object DiamondExtr {
    // Option[(top, ifFalse, ifTrue, cmp)]
    def unapply(bottom: Block): Option[(Block, Block, Block, Cmp)] = {
      if (bottom.getPredCount != 2) return None
      val predBlock1 = bottom.predBlock(0)
      if (predBlock1 == null || predBlock1.getPredCount != 1) return None
      val predBlock2 = bottom.predBlock(1)
      if (predBlock2 == null || predBlock2.getPredCount != 1) return None
      val topCand = predBlock1.predBlock()
      if (topCand != predBlock2.predBlock()) return None

      val (ifFalse, ifTrue) = predBlock1.getPred(0) match {
        case p: Proj if p.getNum == Cond.pnFalse => (predBlock1, predBlock2)
        case p: Proj if p.getNum == Cond.pnTrue => (predBlock2, predBlock1)
        case _ => return None
      }

      // predBlock <- Proj <- Cond <- Cmp
      predBlock1.getPred(0).getPred(0).getPred(0) match {
        case cmp: Cmp => Some((topCand, ifFalse, ifTrue, cmp))
        case _ => None
      }
    }
  }

  /* Detects Cmp nodes with bad branch prediction characteristics */
  object HardToPredictExtr {
    // We'll say that x % y == 0 with y a power of two < 5 and comparisons of Booleans are sufficiently hard to predict
    def unapply(cmp: Cmp): Boolean = (cmp.getPred(0).getMode == Mode.getBu && cmp.getPred(1).getMode == Mode.getBu) ||
      ((cmp.getLeft, cmp.getRight) match {
        case (AndExtr(_, ConstExtr(1 | 2)), ConstExtr(0)) => true
        case _ => false
      })
  }
}
