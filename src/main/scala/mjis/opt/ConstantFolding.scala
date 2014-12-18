package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.mutable
import scala.collection.JavaConversions._

object ConstantFolding extends Optimization {

  override def optimize(g: Graph): Unit = {
    BackEdges.enable(g)
    new ConstantFoldingVisitor(g).foldAndReplace()
    BackEdges.disable(g)
  }

  private class ConstantFoldingVisitor(g: Graph) extends NodeVisitor.Default {

    /** Extends Firm's TargetValue class with some convenience functions **/
    private implicit class TargetValueWithLatticeOps(tval: TargetValue) {

      def sup(others: Iterable[TargetValue]): TargetValue =
        others.foldLeft(tval)((agg, next) => agg.sup(next))

      def sup(other: TargetValue): TargetValue = {
        if (tval == TargetValue.getBad || other == TargetValue.getBad)
          return TargetValue.getBad
        if (tval == TargetValue.getUnknown)
          return other
        if (other == TargetValue.getUnknown || sameConstantAs(other))
          return tval
        TargetValue.getBad
      }

      def sameConstantAs(other: TargetValue): Boolean = {
        tval.isConstant && other.isConstant && tval.asInt == other.asInt
      }

      def ===(other: TargetValue): Boolean =
        tval == other || (tval.isConstant && other.isConstant
          && tval.compare(other) == Relation.Equal)

    }

    // maps node to their values in the lattice
    var nodeToTarval: Map[Node, TargetValue] = Map().withDefaultValue(TargetValue.getUnknown)
    val workList = new mutable.Stack[Node]

    /**
     * Executes constant folding in the irg.
     */
    def foldAndReplace(): Unit = {
      val visitor = new NodeVisitor.Default {
        override def defaultVisit(node: Node): Unit = {
          workList.push(node)
        }
      }
      g.walkTopological(visitor)
      while (workList.nonEmpty) {
        val head = workList.pop()
        head.accept(this)
      }
      // Replace all nodes with const's if possible
      for ((node, targetval) <- nodeToTarval) {
        if (targetval.isConstant) {
          node match {
            case _: Div | _: Mod =>
              // the Div / Mod node itself is not exchanged, instead it's result Proj
              // will be replaced
              Optimization.deleteDivOrMod(node)
            case _: Proj => if (node.getMode != Mode.getX) GraphBase.exchange(node, g.newConst(targetval))
            case cond: Cond =>
              // all successors of cond nodes are proj true / false
              val pred = targetval == TargetValue.getBTrue
              val trueProj = BackEdges.getOuts(node).filter(_.node.asInstanceOf[Proj].getNum == Cond.pnTrue).head.node
              val falseProj = BackEdges.getOuts(node).filter(_.node.asInstanceOf[Proj].getNum == Cond.pnFalse).head.node
              val takenBranch = if (pred) trueProj else falseProj
              val discardedBranch = if (pred) falseProj else trueProj

              GraphBase.exchange(node, g.newBad(Mode.getX))
              GraphBase.exchange(takenBranch, g.newJmp(takenBranch.getBlock))
              GraphBase.exchange(discardedBranch, g.newBad(Mode.getX))

            case _ => GraphBase.exchange(node, g.newConst(targetval))
          }
        }
      }
    }

    override def visit(node: Const): Unit = {
      updateWorkList(node, node.getTarval)
    }

    override def visit(node: Phi): Unit = {
      val merged = TargetValue.getUnknown.sup(node.getPreds.map(nodeToTarval(_)))
      updateWorkList(node, merged)
    }

    override def visit(node: Add): Unit =
      foldBinaryIntOperator((x, y) => x.add(y), node)

    override def visit(node: Sub): Unit =
      foldBinaryIntOperator((x, y) => x.sub(y, Mode.getIs), node)

    override def visit(node: Proj): Unit = {
      if (node.getPred.isInstanceOf[Start]
        || node.getPred.isInstanceOf[Call]
        || node.getPred.isInstanceOf[Load])
        updateWorkList(node, TargetValue.getBad)
      else if (node.getMode == Mode.getIs || node.getMode == Mode.getX)
        foldUnaryIntOperator(x => x, node)
    }

    override def visit(node: Div): Unit = nodeToTarval(node.getPred(1)) match {
      case TargetValueExtr(0) =>
        // 0 / x == 0 (ignoring 0/0)
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ =>
        foldBinaryIntOperator((x, y) => x.div(y), node, node.getPred(1), node.getPred(2))
    }

    override def visit(node: Mod): Unit = nodeToTarval(node.getPred(2)) match {
      case TargetValueExtr(1) =>
        // x % 1 == 0
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ => foldBinaryIntOperator((x, y) => x.mod(y), node, node.getPred(1), node.getPred(2))
    }

    override def visit(node: Mul): Unit = (nodeToTarval(node.getPred(0)), nodeToTarval(node.getPred(1))) match {
      case (TargetValueExtr(0), _) | (_, TargetValueExtr(0)) =>
        // x * 0 == 0 * x == 0
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ => foldBinaryIntOperator((x, y) => x.mul(y), node)
    }

    override def visit(node: Minus): Unit =
      foldUnaryIntOperator(x => x.neg, node)

    override def visit(node: Cmp): Unit = {
      node.getRelation match {
        case Relation.Equal => foldBinaryBooleanOperator((x, y) => x == y, node)
        case Relation.Greater => foldBinaryBooleanOperator((x, y) => x > y, node)
        case Relation.GreaterEqual => foldBinaryBooleanOperator((x, y) => x >= y, node)
        case Relation.Less => foldBinaryBooleanOperator((x, y) => x < y, node)
        case Relation.LessEqual => foldBinaryBooleanOperator((x, y) => x <= y, node)
        case Relation.UnorderedLessGreater => foldBinaryBooleanOperator((x, y) => x != y, node)
        case _ => ???
      }
    }

    override def visit(node: Cond): Unit = {
      val pred = nodeToTarval(node.getSelector)
      updateWorkList(node, pred)
    }

    private def foldUnaryIntOperator(op: TargetValue => TargetValue, node: Node): Unit = {
      val predVal = nodeToTarval(node.getPred(0))
      if (predVal == TargetValue.getBad)
        updateWorkList(node, TargetValue.getBad)
      else if (predVal.isConstant)
        updateWorkList(node, op(predVal))
    }

    private def foldBinaryIntOperator(op: (TargetValue, TargetValue) => TargetValue, node: Node): Unit =
      foldBinaryIntOperator(op, node, node.getPred(0), node.getPred(1))

    private def foldBinaryIntOperator(op: (TargetValue, TargetValue) => TargetValue, node: Node,
                                      leftNode: Node, rightNode: Node): Unit = {
      val left = nodeToTarval(leftNode)
      val right = nodeToTarval(rightNode)
      if (left != TargetValue.getUnknown
        && right != TargetValue.getUnknown) {
        if (left.isConstant && right.isConstant)
          updateWorkList(node, op(left, right))
        else
          updateWorkList(node, TargetValue.getBad)
      }
    }

    private def foldBinaryBooleanOperator(op: (Int, Int) => Boolean, cmp: Cmp): Unit = {
      foldBinaryIntOperator((left, right) =>
        if (op(left.asInt, right.asInt)) TargetValue.getBTrue else TargetValue.getBFalse,
        cmp, cmp.getLeft, cmp.getRight
      )
    }

    private def updateWorkList(node: Node, tarVal: TargetValue): Unit = {
      // Compare old and current value, only update worklist if something changed
      if (!(nodeToTarval(node) === tarVal)) {
        nodeToTarval += node -> tarVal
        workList pushAll BackEdges.getOuts(node).map(_.node)
      }
    }
  }

}