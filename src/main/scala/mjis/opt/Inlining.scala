package mjis.opt

import firm._
import firm.nodes._
import mjis.CallGraph
import mjis.opt.FirmExtensions._

import scala.collection.JavaConversions._
import scala.collection.mutable


object Inlining extends Optimization(needsBackEdges = true) {

  /** We have to take special care not to upset libFirm too much in this step. Certain unwritten assumptions that firm
      has about graphs:
      -- All Start, Const and Address nodes are in the start block
      -- All Proj nodes belong to the block of their predecessor
      -- All blocks must be reachable from the End block
      -- On the moment BackEdges are enabled, all nodes must be reachable from End
      These must hold even for dead code and for nodes not reachable from End. There's no real reason why firm couldn't
      generate code in the latter case, but library-internal sanity checks prevent us from doing that.
   */

  override def optimize(): Boolean = {
    var changed = false
    val graphs = CallGraph.graphsInTopologicalOrder
    for (_ <- 0 until maxDepth)
      changed = graphs.map(optimize).toList.exists(b => b) || changed
    changed
  }

  override def _optimize(g: Graph): Unit = {
    inlineCalls(g)
  }

  private val maxDepth = 3

  def inlineCalls(graph: Graph) = {
    CallGraph.calls(graph).foreach(call =>
      if (shouldInline(call)) {
        changed = true
        val (preCallBlock, postCallBlock) = splitBlockAlong(call)
        val callee = call.getCalledGraph.get
        val cloner = new NodeCloner (graph, callee)
        callee.walkTopological(cloner)
        cloner.finish()

        rewire(call, cloner, preCallBlock, postCallBlock)
      }
    )
  }

  // TODO SCIENCE to figure out a good value for this
  private def maxNodeNum = 100

  def shouldInline(call: Call): Boolean = {
    call.getCalledGraph match {
      // callee is null in calls to stdlib functions (e.g. calloc, System.out.println)
      case None => false
      case Some(graph) =>
        // There's little point in inlining calls to recursive methods
        if (graph != call.getGraph && CallGraph.isRecursive(graph)) false
        // Graph.getLastIdx should give us the number of nodes constructed in the graph
        else if (graph.getLastIdx <= maxNodeNum) true
        else
          // since optimizations may remove nodes, we still have to actually count them
          // TODO DCE so we don't count dead code
          NodeCollector.fromWalk(graph.walk).size < maxNodeNum
    }
  }

  /** redirects the edge `from`-->`to` to `from`-->`newTo` */
 private def redirectEdge(from: Node, to: Node, newTo: Node): Unit = {
    for ((`to`, idx) <- from.getPreds.zipWithIndex) from.setPred(idx, newTo)
    if (from.isInstanceOf[Proj]) {
      from.setBlock(newTo.getBlock)
      // To keep libFirm from complaining. Shouldn't hurt code generation if this is disabled though
      from.successors.filter(_.isInstanceOf[Proj]).map(_.setBlock(from.getBlock))
    }
  }

  private def moveNodesAndPreds(node: Node, oldBlock: Node, newBlock: Node): Unit = {
    if (node.getBlock == oldBlock) {
      node.setBlock(newBlock)
      node.getPreds.foreach(n => moveNodesAndPreds(n, oldBlock, newBlock))
    }
  }

  // Moves the "upper" part of the `node`'s block to a new block.
  // Must keep validity of the CFG invariant, as inlining will happen right after this step
  private def splitBlockAlong(node: Node): (Block, Block) = {
    val oldBlock = node.block
    val graph = node.getGraph
    val newBlock = graph.newBlock(oldBlock.getPreds.toArray).asInstanceOf[Block]

    moveNodesAndPreds(node, oldBlock, newBlock)
    if (graph.getStartBlock == oldBlock) {
      graph.setStartBlock(newBlock)
      graph.getNoMem.setBlock(graph.getStartBlock)
      // an unreachable parameter proj or const node may be lurking in the old start block.
      oldBlock.successors.foreach({
        case n@(_: Const | _: Address) => n.setBlock(newBlock)
        case proj: Proj => proj.setBlock(proj.getPred(0).getBlock)
        case _ => ()
      })
    }

    // The caller must be intact when we copy the callee's nodes -- caller and callee may be the same graph
    // (inlined recursive calls). For that reason we "fix" the graph temporarily.
    // TODO we could just check for recursive calls
    val jmp = graph.newJmp(newBlock)
    oldBlock.setPreds(Array(jmp))

    oldBlock.successors.filter(_.isInstanceOf[Phi]).map(_.setBlock(newBlock))
    (newBlock, oldBlock)
  }

  private def rewire(call: Call, copy: NodeCloner, preCallBlock: Block, postCallBlock: Block) = {
    val graph = call.getGraph

    val jmp = graph.newJmp(preCallBlock)
    copy.copiedStartBlock.setPreds((copy.copiedStartBlock.getPreds ++ Seq(jmp)).toArray)

    copy.keepaliveNodes.foreach(graph.keepAlive)

    // control flow
    val jmps = mutable.ListBuffer[Node]()
    for (node <- copy.returnNodes) {
      val jmp = graph.newJmp(node.getBlock)
      jmps += jmp
    }
    // when creating memory phis, the number of predecessors of this block must match the number of phi inputs
    postCallBlock.setPreds(jmps.toArray)

    // wire up arguments. The `+ 2` is to skip the call's first two predecessors: its function address and memory node.
    copy.argEdges.foreach({ case (from, to) => redirectEdge(from, to, call.getPred(to.asInstanceOf[Proj].getNum + 2)) })

    // wire up start memory state
    val callMem = call.getPred(0)
    copy.startMemEdges.foreach({ case (from, to) => redirectEdge(from, to, callMem) })

    // wire up end memory state
    val endMemNodes = copy.returnNodes.map(_.getPred(0)).toArray
    val memPhi = if (endMemNodes.length == 1) endMemNodes.head else graph.newPhi(postCallBlock, endMemNodes, Mode.getM)
    val callMProj = call.successors.filter(_.getMode == Mode.getM).head
    callMProj.successors.foreach(n => {
      redirectEdge(n, callMProj, memPhi)
    })

    // wire up return value
    val returnVals = copy.returnNodes.flatMap(_.getPreds).filter(_.getMode != Mode.getM).toList
    if (returnVals.nonEmpty) {
      val inlinedResultNode: Node = if (returnVals.length > 1)
          graph.newPhi(postCallBlock, returnVals.toArray[Node], returnVals.head.getMode)
        else
          returnVals.head
      // get the result tuple node of the original call
      val callResultTuple = call.successors.find(n => n.isInstanceOf[Proj] && n.getMode == Mode.getT)
      callResultTuple match {
        case None => () // no one cares about the return value
        case Some(tuple) =>
          // we only ever have one return value
          val callResult = tuple.successors.head
          val resultUsers = callResult.successors
          for ((node, idx) <- resultUsers.zipWithIndex)
            redirectEdge(node, callResult, inlinedResultNode)
      }
    }

    // As libFirm is *very specific* about which nodes it wants to see
    // in the start block, we need to walk the whole graph to find all the odd and about Const or Addr nodes.
    graph.walkTopological(new NodeVisitor.Default {
      val startBlock = graph.getStartBlock
      override def visit(const: Const) = const.setBlock(startBlock)
      override def visit(addr: Address) = addr.setBlock(startBlock)
      override def visit(proj: Proj) = {
        proj.setBlock(proj.getPred(0).block)
      }
    })

  }

}
