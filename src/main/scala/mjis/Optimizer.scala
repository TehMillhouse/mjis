package mjis

import firm._
import java.io.BufferedWriter
import firm.nodes.Bad
import mjis.util.FirmDumpHelper
import mjis.opt._
import mjis.opt.FirmExtensions._
import scala.collection.JavaConversions._

class Optimizer(input: Unit, config: Config) extends Phase[Unit] {

  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "Optimizer"))
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

  var highLevelOptimizations = List(LoopStrengthReduction, RedundantLoadElimination)
  var generalOptimizations = List(
    ConstantFolding, Normalization, CommonSubexpressionElimination, TrivialPhiElimination, Identities)
  // The following optimizations mustn't be iterated with other optimizations
  // because of possible interactions leading to infinite loops
  var volatileOptimizations = if (config.inlining) List(Inlining, LoopUnrolling) else List(LoopUnrolling)

  def exec(optimizations: List[Optimization]): Unit = {
    // always run all optimizations
    while (optimizations.map(_.optimize()).exists(b => b)) {}
  }

  override protected def getResult(): Unit = {
    exec(generalOptimizations ++ highLevelOptimizations)
    volatileOptimizations.foreach(_.optimize())
    Util.lowerSels()
    exec(generalOptimizations)

    Program.getGraphs.foreach(removeCriticalEdges)
  }

}
