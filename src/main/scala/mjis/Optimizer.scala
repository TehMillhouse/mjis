package mjis

import firm._
import java.io.BufferedWriter
import firm.nodes.Bad
import mjis.util.{Digraph, FirmDumpHelper}
import mjis.opt._
import mjis.opt.FirmExtensions._
import scala.collection.JavaConversions._

class Optimizer(input: Unit) extends Phase[Unit] {

  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-Optimizer"))
  }

  def removeCriticalEdges(g: Graph): Unit = {
    val cfGraph = g.getBlockGraph.transposed
    for (block <- NodeCollector.fromBlockWalk(g.walkBlocks))
      if (block.getPreds.count(!_.isInstanceOf[Bad]) > 1)
        for ((pred, idx) <- block.getPreds.zipWithIndex if !pred.isInstanceOf[Bad])
          if (cfGraph.edges(pred.block).size > 1) {
            val newBlock = g.newBlock(Array(pred))
            block.setPred(idx, g.newJmp(newBlock))
          }
  }

  val highLevelOptimizations = List(LoopStrengthReduction, RedundantLoadElimination, Inlining)
  val generalOptimizations = List(
    ConstantFolding, Normalization, CommonSubexpressionElimination, TrivialPhiElimination, Identities)

  def exec(optimizations: List[Optimization]): Unit = {
    // always run all optimizations
    while (optimizations.map(_.optimize()).exists(b => b)) {}
  }

  override protected def getResult(): Unit = {
    exec(generalOptimizations ++ highLevelOptimizations)
    Util.lowerSels()
    exec(generalOptimizations)

    Program.getGraphs.foreach(removeCriticalEdges)
  }

}
