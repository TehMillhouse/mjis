package mjis

import firm._
import firm.nodes.NodeVisitor.Default
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.util.MapExtensions._
import mjis.util.Digraph

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object CallGraph {
  private def main = Program.getGraphs.find(_.getEntity.getLdName == "__main").get

  // Optimizations may eliminate calls, so be careful not to trust the information gotten
  // from this for longer than one optimization phase
  def calls(graph: Graph) = {
    val calls = mutable.ListBuffer[Call]()
    graph.walkTopological(new Default {
      override def visit(call: Call) = { calls += call }
    })
    calls
  }

  def isRecursive(graph: Graph) = {
    calls(graph).flatMap(_.getCalledGraph).contains(graph)
  }

  def graphsInTopologicalOrder() = {
    val m = new mutable.HashMap[Graph, Seq[Graph]]().withPersistentDefault(g => calls(g).flatMap(_.getCalledGraph).toSeq)
    new Digraph[Graph](m).getTopologicalSorting(main)
  }
}
