package mjis.opt

import firm._
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.util.MapExtensions._
import mjis.util._

import scala.collection.mutable
import scala.collection.JavaConversions._

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


  private val maxLoopNodeNum = 30
  def shouldUnroll(loop: Seq[Block]): Boolean = {
    loop.map(BackEdges.getNOuts(_)).sum < maxLoopNodeNum
  }

  override def _optimize(graph: Graph): Unit = {
    val inductionVars /* "IVs" */ = graph.getInductionVariables.toSet
    // nothing to do. Should be true for most graphs
    if (inductionVars.isEmpty) return
    val successorBlocks = graph.getBlockGraph.transposed
    val SCCTree = successorBlocks.getSCCTree(graph.getStartBlock)
    val innerLoops = InnermostLoops(SCCTree, inductionVars.map(_.value.getBlock.asInstanceOf[Block]))
    val unrollLoops = innerLoops.filter(l => shouldUnroll(l.tree.collect({
      case b if !b.nodes.contains(l.dominator) => b.nodes
    }).flatten))

    println(unrollLoops)
    copyBlocks(unrollLoops.map(l => l.dominator))
   }

  private def copyBlocks(blocks: Seq[Block]): mutable.Map[Node, Node] = {
    val copies = mutable.HashMap[Node, Node]().withPersistentDefault({ n =>
      if (n.getBlock == null || blocks.contains(n.getBlock))
        n.getGraph.copyNode(n)
      else
        n
    })
    blocks.foreach(block => {
      block.successors.foreach(n => {
        copies(n).getPreds.zipWithIndex.foreach({case (pred, idx) => copies(n).setPred(idx, copies(pred))})
      })
    })
    copies
  }
}
