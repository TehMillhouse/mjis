package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._


object ConstantFolding extends Optimization {

  private val unknown = TargetValue.getUnknown
  private val conflicting = TargetValue.getBad

  private def sup(x: TargetValue, y: TargetValue): TargetValue = (x, y) match {
    case (`unknown`, _) => y
    case (_, `unknown`) => x
    case (TargetValueExtr(i), TargetValueExtr(j)) if i == j => x
    case _ => conflicting
  }

  private def sup(tarvals: Iterable[TargetValue]): TargetValue = tarvals.foldLeft(unknown)(sup)

  private def liftUnary(op: TargetValue => TargetValue, tarval: TargetValue): TargetValue = {
    if (tarval.isConstant)
      op(tarval)
    else
      tarval
  }

  private def liftBinary(op: (TargetValue, TargetValue) => TargetValue, left: TargetValue, right: TargetValue): TargetValue = (left, right) match {
    case (TargetValueExtr(_), TargetValueExtr(_)) => op(left, right)
    case _ => sup(left, right)
  }

  private def fromBool(b: Boolean): TargetValue = if (b) TargetValue.getBTrue else TargetValue.getBFalse

  override def optimize(g: Graph): Unit = {
    BackEdges.enable(g)

    val tarvals = DataFlowAnalysis.iterate[TargetValue](g, unknown, (node, values) => {
      def liftBin(op: (TargetValue, TargetValue) => TargetValue) = liftBinary(op, values(0), values(1))

      node match {
        case c: Const => c.getTarval
        case _: Phi => sup(values)
        case _: Add => liftBin(_.add(_))
        case _: Sub => liftBin(_.sub(_, node.getMode))
        case ProjExtr(_: Cond, Cond.pnFalse) => liftUnary(_.not, values(0))
        case ProjExtr(_: Cond, Cond.pnTrue)
             | ProjExtr(_: Mod, Mod.pnRes)
             | ProjExtr(_: Div, Div.pnRes)
             | _: Cond => values(0)
        case _: Div => values(1) match {
          // 0 / x == 0 (ignoring 0/0)
          case TargetValueExtr(0) => new TargetValue(0, Mode.getIs)
          case _ => liftBinary(_.div(_), values(1), values(2))
        }
        case _: Mod => values(2) match {
          // x % 1 == 0
          case TargetValueExtr(1) => new TargetValue(0, Mode.getIs)
          case _ => liftBinary(_.mod(_), values(1), values(2))
        }
        case _: Mul => values match {
          // x * 0 == 0 * x == 0
          case List(TargetValueExtr(0), _) | List(_, TargetValueExtr(0)) => new TargetValue(0, Mode.getIs)
          case _ => liftBin(_.mul(_))
        }
        case _: Minus => liftUnary(_.neg, values(0))
        case cmp: Cmp => liftBin((x, y) => fromBool(x.compare(y).contains(cmp.getRelation)))
        case _ => conflicting
      }
    })

    // Replace all nodes with const's if possible
    for ((node, tarval) <- tarvals) {
      if (tarval.isConstant) {
        node match {
          case _: Const =>
          case _: Div | _: Mod =>
            // the Div / Mod node itself is not exchanged, instead its result Proj
            // will be replaced
            g.deleteDivOrMod(node)
          case _: Proj if node.getMode == Mode.getX =>
            if (tarval == TargetValue.getBTrue)
              GraphBase.exchange(node, g.newJmp(node.getBlock))
            else
              GraphBase.exchange(node, g.newBad(Mode.getX))
          case _ => GraphBase.exchange(node, g.newConst(tarval))
        }
      }
    }

    BackEdges.disable(g)
  }

}
