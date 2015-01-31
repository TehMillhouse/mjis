package mjis.opt

import firm._
import firm.nodes._
import mjis.util.MapExtensions._
import mjis.opt.FirmExtensions._

import scala.collection.mutable.{ListBuffer, HashMap, Map}
import scala.collection.JavaConversions._

private class NodeCloner (caller: Graph, callee: Graph) extends NodeVisitor.Default {
  // When inlining recursive calls, BackEdges are already enabled
  private val backEdgesWereDisabled = !BackEdges.enabled(callee)
  var calleeStart: Node = null
  var copiedStartBlock: Block = null
  // Edges (in form of tuples) from each usage of an argument to its corresponding Proj node below the argument tuple
  val argEdges: ListBuffer[(Node, Node)] = ListBuffer()
  var startMemEdges: Iterable[(Node, Node)] = null
  val returnNodes: ListBuffer[Return] = ListBuffer()
  var keepaliveNodes: Iterable[Node] = null
  private val copies: Map[Node, Node] = HashMap[Node, Node]().withPersistentDefault {
    case _: NoMem =>
      caller.getNoMem
    case node@(_: Block | _: Anchor) => caller.copyNode(node)
    case node =>
      val newNode = caller.copyNode(node)
      newNode.setBlock(copies(node.getBlock))
      newNode
  }

  if (backEdgesWereDisabled)
    BackEdges.enable(callee)

  def finish() = {
    // Now that all nodes were copied, get information we'll need for rewiring the inlined method
    val startMem = callee.getInitialMem
    startMemEdges = startMem.successors.map(e => (copies(e), copies(startMem)))

    callee.getArgs.successors.foreach(arg =>
      if (!arg.isInstanceOf[Anchor])
        argEdges ++= arg.successors.map(user => (copies(user), copies(arg)))
    )

    if (backEdgesWereDisabled)
      BackEdges.disable(callee)
  }

  override def defaultVisit(node: Node) = {
    val newNode = copies(node)
    for (n <- 0 until node.getPredCount)
      newNode.setPred(n, copies(node.getPred(n)))
    if (!node.isInstanceOf[Block])
      newNode.setBlock(copies(node.getBlock))
    else if (callee.getStartBlock == node) copiedStartBlock = copies(node).asInstanceOf[Block]
  }

  override def visit(start: Start) = {
    calleeStart = start
  }

  override def visit(end: End) = {
    defaultVisit(end)
    keepaliveNodes = end.getPreds.map(copies(_))
  }

  override def visit(node: Return) = {
    defaultVisit(node)
    returnNodes += copies(node).asInstanceOf[Return]
  }

  // the following is all just to shut libFirm up and bears no real significance to inlining semantics
  override def visit(const: Const) = {
    defaultVisit(const)
    copies(const).setBlock(caller.getStartBlock)
  }
  override def visit(addr: Address) = {
    defaultVisit(addr)
    copies(addr).setBlock(caller.getStartBlock)
  }
  override def visit(proj: Proj) = {
    defaultVisit(proj)
    copies(proj).setBlock(copies(proj).getPred(0).getBlock)
  }

}
