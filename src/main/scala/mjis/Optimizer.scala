package mjis

import firm._
import java.io.BufferedWriter
import firm.nodes.Bad
import mjis.util.FirmDumpHelper
import mjis.opt._
import mjis.opt.FirmExtensions._
import scala.collection.JavaConversions._

class Optimizer(input: Unit, config: Config = Config()) extends Phase[Unit] {

  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "Optimizer"))
  }

  def removeCriticalEdges(g: Graph): Unit = {
    val cfGraph = g.getBlockGraph.transposed
    g.walkBlocksWith { block =>
      if (block.getPreds.count(!_.isInstanceOf[Bad]) > 1)
        for ((pred, idx) <- block.getPreds.zipWithIndex if !pred.isInstanceOf[Bad])
          if (cfGraph.edges(pred.block).size > 1) {
            val newBlock = g.newBlock(Array(pred))
            block.setPred(idx, g.newJmp(newBlock))
          }
    }
  }

  var highLevelOptimizations = List(
    LoopStrengthReduction, RedundantLoadElimination, UnusedParameterElimination,
    PureFunctionCallElimination, LoopInvariantCodeMotion)
  var generalOptimizations = List(
    ConstantFolding, Normalization, CommonSubexpressionElimination, TrivialPhiElimination, Identities) ++
    (if (!config.useFirmBackend) Seq(ConditionalMoves) else Seq())
  // The following optimizations mustn't be iterated with other optimizations
  // because of possible interactions leading to infinite loops
  var volatileOptimizations = if (config.inlining) List(Inlining, LoopUnrolling) else List(LoopUnrolling)

  def exec(optimizations: List[Optimization]): Unit = {
    // always run all optimizations
    while (optimizations.map(_.optimize()).exists(b => b)) {}
  }

  override protected def getResult(): Unit = {
    exec(generalOptimizations ++ highLevelOptimizations)
    Program.getGraphs.foreach(g => {
      bindings.binding_irgopt.remove_bads(g.ptr)
      bindings.binding_irgopt.remove_unreachable_code(g.ptr)
    })
    volatileOptimizations.foreach(_.optimize())
    exec(generalOptimizations ++ highLevelOptimizations)

    Program.getGraphs.foreach(removeCriticalEdges)
    Program.getGraphs.foreach(g => {
      bindings.binding_irgopt.remove_bads(g.ptr)
      bindings.binding_irgopt.remove_unreachable_code(g.ptr)
    })

    if (config.firmDump)
      dumpResult(null)
  }

}
