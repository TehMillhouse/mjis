package mjis.opt

import firm._
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.util._

import scala.collection.mutable

object LoopUnrolling extends Optimization(needsBackEdges = true) {

  /** We only want to unroll the innermost loops.
    * Those are those IVs X which dominate no other SCC */
  def InnermostLoops(sccTree: Seq[SCCTreeNode[Block]], IVBlocks: Set[Block]): Seq[SCCLoop[Block]] =
    sccTree.collect({
      case loop: SCCLoop[Block]
        if IVBlocks(loop.dominator) && !loop.tree.exists(_.isInstanceOf[SCCLoop[Block]]) =>
          Seq(loop)
      case loop: SCCLoop[Block] => InnermostLoops(loop.tree, IVBlocks)
    }).flatten

  def shouldUnroll(loop: Seq[Block]): Boolean = {
    loop.map(BackEdges.getNOuts(_)).sum < 30
  }

  override def _optimize(graph: Graph): Unit = {
    val inductionVars /* "IVs" */ = graph.getInductionVariables.toSet
    // nothing to do. Should be true for most graphs
    if (inductionVars.isEmpty) return
    val successorBlocks = graph.getBlockGraph.transposed
    val SCCTree = successorBlocks.getSCCTree(graph.getStartBlock)
    val innerLoops = InnermostLoops(SCCTree, inductionVars.map(_.value.getBlock.asInstanceOf[Block]))
    innerLoops.foreach(l => println(shouldUnroll(l.tree.map(_.nodes).flatten)))


   }
}
