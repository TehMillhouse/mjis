package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.util.Digraph
import scala.collection.JavaConversions._
import scala.collection.immutable.ListMap

/**
 * value = Phi(start, incrAdd)
 * ...
 * incrAdd = Add(value, incr)
 */
case class InductionVariable(value: Phi, start: Const, incr: Const, incrAdd: Add)

object FirmExtensions {

  implicit class GraphExt(g: Graph) {

    def getBlockGraph: Digraph[Block] = new Digraph(ListMap(
      NodeCollector.fromBlockWalk(g.walkBlocks)
      .map(b => b -> b.getPreds.map(_.block).filter(_ != null).toSeq): _*
    ))

    def getDominators: Map[Block, Set[Block]] = {
      DataFlowAnalysis.iterateBlocks[Set[Block]](g, NodeCollector.fromBlockWalk(g.walkBlocks).toSet,
        (node, dominators) => dominators match {
          case Nil => Set(node)
          case _ => dominators.reduce(_.intersect(_)) + node
        }
      )
    }

    def getInductionVariables: Seq[InductionVariable] = {
      // TODO: recognize loop-invariant start and incr nodes instead of just constant ones
      NodeCollector.fromWalk(g.walk).map(node => node match {
        case PhiExtr(start: Const, incrAdd@AddExtr(`node`, incr: Const)) =>
          Some(InductionVariable(node.asInstanceOf[Phi], start, incr, incrAdd.asInstanceOf[Add]))
        case _ => None
      }).flatten
    }

    def methodType: MethodType = g.getEntity.getType.asInstanceOf[MethodType]

  }

  implicit class CallExt(call: Call) {
    def getCalledGraph: Option[Graph] = {
      val ent = call.getPtr.asInstanceOf[Address].getEntity
      if (ent == null)
        None
      else
        Option(ent.getGraph)
    }
  }

  implicit class NodeExt(node: Node) {

    def block: Block = node match {
      case _: Bad => null
      case _ => node.getBlock match {
        case b: Block => b
        case _ => null
      }
    }

    def successors: Iterable[Node] = {
      BackEdges.getOuts(node).map(_.node)
    }

    def idx: Int = bindings.binding_irnode.get_irn_idx(node.ptr)

  }

}
