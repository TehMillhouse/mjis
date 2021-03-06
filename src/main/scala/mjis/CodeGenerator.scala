package mjis

import java.io._

import firm._
import firm.nodes.{Bad, Node, Block}
import mjis.asm._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.opt.NodeCollector
import mjis.util.MapExtensions._
import mjis.asm.AMD64Registers._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

object CodeGenerator {
  def align(x: Int, alignment: Int = 8) = if (x % alignment == 0) x else x + (alignment - x % alignment)
}

class CodeGenerator(a: Unit) extends Phase[AsmProgram] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    a.write(new MjisAssemblerFileGenerator(result, null).generateCode())
  }
  val resultProgram = new AsmProgram()

  override def getResult(): AsmProgram = {
    Program.getGraphs.foreach(g => {
      resultProgram.functions += new MethodCodeGenerator(g).getResult()
    })
    resultProgram
  }

  def intConstOp(i: Int): Operand = ConstOperand(i, Mode.getIs.getSizeBytes)
  def regOp(node: Node): RegisterOperand = RegisterOperand(node.idx, node match {
    case n : nodes.Call =>
      val methodEntity = n.getPtr.asInstanceOf[nodes.Address].getEntity
      val methodType = methodEntity.getType.asInstanceOf[MethodType]
      assert(methodType.getNRess > 0)
      methodType.getResType(0).getSizeBytes
    case n : nodes.Load => n.getLoadMode.getSizeBytes
    case n : firm.nodes.Div => n.getResmode.getSizeBytes
    case n : firm.nodes.Mod => n.getResmode.getSizeBytes
    case _ => node.getMode.getSizeBytes
  })

  class MethodCodeGenerator(g: Graph) {
    val basicBlocks = mutable.ListMap[Block, AsmBasicBlock]().
      withPersistentDefault(b => new AsmBasicBlock(if (b == null) -1 else b.getNr))
    val function = new AsmFunction(g.getEntity.getLdName)

    val usedParamRegisters = mutable.Map[Int, Node]()
    val toVisit = mutable.Queue[Node]()

    def getResult(): AsmFunction = {
      val backEdgesWereEnabled = BackEdges.enabled(g)
      BackEdges.enable(g)

      val instructions = mutable.Map[Node, Seq[Instruction]]()

      for (n <- NodeCollector.fromWalk(g.walkTopological)) {
        val isBlockRoot = BackEdges.getOuts(n).exists(e => e.node.block != null && e.node.block != n.block)
        if (isBlockRoot)
          instructions(n) = createValue(n)
      }
      while (toVisit.nonEmpty) {
        val n = toVisit.dequeue()
        if (!instructions.contains(n))
          instructions(n) = createValue(n)
      }

      val firmBlocks = NodeCollector.getBlocksInReverseBackEdgesPostOrder(g).map(basicBlocks).toList

      firmBlocks.head.predecessors += Some(function.prologue)
      function.prologue.successors += firmBlocks.head

      firmBlocks.last.successors += function.epilogue
      function.epilogue.predecessors += Some(firmBlocks.last)

      function.basicBlocks = function.prologue :: firmBlocks ++ Seq(function.epilogue)

      for ((firmBlock, asmBlock) <- basicBlocks) {
        asmBlock.predecessors ++= firmBlock.getPreds.map(b =>
          if (b.isInstanceOf[Bad] || !function.basicBlocks.contains(basicBlocks(b.block))) None
          else Some(basicBlocks(b.block)))
      }

      val nextBlock = function.basicBlocks.zip(function.basicBlocks.tail).toMap

      for (n <- NodeCollector.fromWalk(g.walkTopological)) {
        val basicBlock = basicBlocks(n.block)
        basicBlock.instructions ++= instructions.getOrElse(n, Seq()) map(_.withComment(s" - $n"))
        basicBlock.controlFlowInstructions ++= createControlFlow(n, nextBlock.get(basicBlock)) map(_.withComment(s" - $n"))
      }
      if (!backEdgesWereEnabled) BackEdges.disable(g)

      // spill param registers early and in a fixed order
      for (reg <- ParamRegisters)
        usedParamRegisters.get(reg).foreach {
          n => function.prologue.instructions += Mov(RegisterOperand(reg, n.getMode.getSizeBytes), regOp(n))
        }

      function.epilogue.controlFlowInstructions +=
        (if (g.methodType.getNRess > 0) Ret(g.methodType.getResType(0).getSizeBytes) else Ret)
      function
    }

    private def createControlFlow(node: Node, nextBlock: Option[AsmBasicBlock]): Seq[Instruction] = {
      def successorBlock(node: Node) = BackEdges.getOuts(node).iterator().next().node.asInstanceOf[Block]
      def successorBlockOperand(node: Node) = new BasicBlockOperand(basicBlocks(successorBlock(node)))
      def isFallthrough(node: Node) = Some(successorBlock(node).getNr) == nextBlock.map(_.nr)
      def jmp(node: Node) =
        if (isFallthrough(node)) Seq()
        else Seq(asm.Jmp(successorBlockOperand(node)))

      node match {
        case _: nodes.Jmp =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          jmp(node)

        case ReturnExtr(retvalOption) =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          (retvalOption match {
            case Some(retval) => Seq(Mov(getOperand(retval), RegisterOperand(RAX, g.methodType.getResType(0).getSizeBytes)))
            case None => Seq()
          }) ++ jmp(node)

        case CondExtr(cmp: nodes.Cmp) =>
          val result = mutable.ListBuffer[Instruction]()

          // Const operand must be on the left
          val (leftOp, rightOp, relation) = getOperand(cmp.getLeft) match {
            case left: ConstOperand => (getOperand(cmp.getRight), left, cmp.getRelation.inversed())
            case left: Operand => (left, getOperand(cmp.getRight), cmp.getRelation)
          }
          result += asm.Cmp(rightOp, leftOp).withComment(s"Evaluate $cmp (actual relation: $relation)")

          val successors = BackEdges.getOuts(node).map(_.node.asInstanceOf[nodes.Proj]).toList
          val projTrue = successors.find(_.getNum == nodes.Cond.pnTrue)
          val projFalse = successors.find(_.getNum == nodes.Cond.pnFalse)
          assert(projTrue.isDefined && projFalse.isDefined)

          basicBlocks(node.block).successors += basicBlocks(successorBlock(projTrue.get))
          basicBlocks(node.block).successors += basicBlocks(successorBlock(projFalse.get))

          // Prefer fallthrough. It just so happens that this will use unconditional jumps for loop iterations.
          if (isFallthrough(projTrue.get)) {
            result += JmpConditional(successorBlockOperand(projFalse.get), relation, negate = true)
            result ++= jmp(projTrue.get)
          } else {
            result += JmpConditional(successorBlockOperand(projTrue.get), relation, negate = false)
            result ++= jmp(projFalse.get)
          }

          result

        case _ => Seq()
      }
    }

    private def getDivModCode(n: Node, left: Node, right: Node): Seq[Instruction] = {
      assert(left.getMode == Mode.getIs)
      assert(right.getMode == Mode.getIs)
      val rightOp = getOperand(right)
      Seq(
        Mov(getOperand(left), RegisterOperand(RAX, 4)),
        Cdq(4) /* sign-extend eax into edx:eax */) ++
      // IDiv cannot handle a constant as right operand
      (if (rightOp.isInstanceOf[ConstOperand])
        Seq(Mov(rightOp, regOp(n)), IDiv(regOp(n)))
      else
        Seq(IDiv(rightOp)))
    }

    private def createValue(node: Node): Seq[Instruction] = {
      def getAddressOperand(node: Node, sizeBytes: Int): AddressOperand = node match {
        // TODO: Use both displacement and offset?
        case AddExtr(base, ConstExtr(displacement)) =>
          toVisit += base
          AddressOperand(
            base = Some(getOperand(base)),
            displacement = displacement,
            sizeBytes = sizeBytes)
        case AddExtr(base, ShlExtr(offset, ConstExtr(shift@(1 | 2 | 3)))) =>
          toVisit ++= Seq(base, offset)
          AddressOperand(
            base = Some(getOperand(base)),
            offset = Some(getOperand(offset)),
            scale = 1 << shift,
            sizeBytes = sizeBytes)
        case AddExtr(ShlExtr(offset, ConstExtr(shift@(1 | 2 | 3))), base) =>
          toVisit ++= Seq(base, offset)
          AddressOperand(
            base = Some(getOperand(base)),
            offset = Some(getOperand(offset)),
            scale = 1 << shift,
            sizeBytes = sizeBytes)
        case AddExtr(base, offset) =>
          toVisit ++= Seq(base, offset)
          AddressOperand(
            base = Some(getOperand(base)),
            offset = Some(getOperand(offset)),
            sizeBytes = sizeBytes)
        case _ =>
          toVisit += node
          AddressOperand(base = Some(getOperand(node)), sizeBytes = sizeBytes)
      }

      node match {
        // special tree patterns that don't necessarily visit all predecessors

        case n@AddExtr(incr, ConstExtr(1)) =>
          toVisit ++= Seq(incr)
          Seq(Mov(getOperand(incr), regOp(n)), Inc(regOp(n)))
        case n@AddExtr(incr, ConstExtr(-1)) =>
          toVisit ++= Seq(incr)
          Seq(Mov(getOperand(incr), regOp(n)), Dec(regOp(n)))
        case n: nodes.Add => Seq(Lea(getAddressOperand(n, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Load =>
          toVisit ++= Seq(n.getMem)
          Seq(Mov(getAddressOperand(n.getPtr, n.getLoadMode.getSizeBytes), regOp(n)))
        case n: nodes.Store =>
          toVisit ++= Seq(n.getMem, n.getValue)
          Seq(Mov(getOperand(n.getValue), getAddressOperand(n.getPtr, n.getType.getSizeBytes)))

        case _ =>
          // non-special patterns
          toVisit ++= node.getPreds
          node match {
            case n: nodes.And => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.And(getOperand(n.getRight), regOp(n)))
            case n: nodes.Add => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.Add(getOperand(n.getRight), regOp(n)))
            case n: nodes.Sub => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.Sub(getOperand(n.getRight), regOp(n)))
            case n: nodes.Minus => Seq(Mov(getOperand(n.getOp), regOp(n)), Neg(regOp(n)))

            case n : nodes.Mul =>
              val tempRegister = RegisterOperand(RAX, n.getMode.getSizeBytes)
              // Normalization moves constants to the right of a Mul node,
              // but the Mul instruction cannot take a constant as operand.
              // Avoid having to allocate an extra register by swapping left and right.
              Seq(Mov(getOperand(n.getRight), tempRegister), asm.Mul(getOperand(n.getLeft)),
                Mov(tempRegister, regOp(n)))

            case n : firm.nodes.Div => getDivModCode(n, n.getLeft, n.getRight) ++ Seq(Mov(RegisterOperand(RAX, 4), regOp(n)))
            case n : firm.nodes.Mod => getDivModCode(n, n.getLeft, n.getRight) ++ Seq(Mov(RegisterOperand(RDX, 4), regOp(n)))

            case n@ShlExtr(x, c@ConstExtr(shift)) => Seq(Mov(getOperand(x), regOp(n)),
              Shl(ConstOperand(shift, c.getMode.getSizeBytes), regOp(n)))

            case n: nodes.Conv => Seq(Mov(getOperand(n.getOp), regOp(n)))

            case n@CallExtr(address, params) =>
              val resultInstrs = ListBuffer[Instruction]()

              val (regParams, stackParams) = params.splitAt(ParamRegisters.length)
              val regSrcOperands = regParams.map(getOperand)
              val paramRegisters = regSrcOperands.zip(ParamRegisters).map {
                case (srcOp, reg) => RegisterOperand(reg, srcOp.sizeBytes)
              }

              for ((srcOp, destOp) <- regSrcOperands zip paramRegisters) {
                resultInstrs += Mov(srcOp, destOp).withComment(s"Load register argument")
              }

              val stackPointerDisplacement = 8 * stackParams.length
              if (stackParams.nonEmpty) {
                resultInstrs += Sub(intConstOp(stackPointerDisplacement), RegisterOperand(RSP, 8)).withComment(s"Move stack pointer for parameters")
                // Push rest of parameters onto the stack in reverse order
                for ((param, i) <- stackParams.zipWithIndex.reverse) {
                  val srcOp = getOperand(param)
                  val tempOp = RegisterOperand(n.idx, srcOp.sizeBytes) // needed if srcOp is a memory operand
                  resultInstrs += Mov(srcOp, tempOp).
                    withComment(s"Reload stack argument").
                    withStackPointerDisplacement(stackPointerDisplacement)
                  resultInstrs += Mov(tempOp, AddressOperand(
                    base = Some(RegisterOperand(RSP, 8)),
                    displacement = 8 * i,
                    sizeBytes = srcOp.sizeBytes)).
                    withComment(s"Push stack argument").
                    withStackPointerDisplacement(stackPointerDisplacement)
                }
              }

              val methodName = address.getEntity.getLdName
              val methodType = address.getEntity.getType.asInstanceOf[MethodType]
              val returnsValue = methodType.getNRess > 0

              resultInstrs +=
                (if (returnsValue) mjis.asm.CallWithReturn(LabelOperand(methodName), methodType.getResType(0).getSizeBytes, paramRegisters)
                else mjis.asm.Call(LabelOperand(methodName), paramRegisters))

              if (stackParams.nonEmpty)
                resultInstrs += Add(intConstOp(stackPointerDisplacement), RegisterOperand(RSP, 8)).withComment(s"Restore stack pointer")

              if (returnsValue) {
                resultInstrs += Mov(RegisterOperand(RAX, methodType.getResType(0).getSizeBytes), regOp(n))
              }

              resultInstrs

            case n: nodes.Phi =>
              if (n.getMode != Mode.getM && n.getMode != Mode.getX)
                basicBlocks(n.block).phis += Phi(n.getPreds.map(getOperand).toSeq, regOp(n))
              Seq()

            case n@ProjExtr(ProjExtr(_, nodes.Start.pnTArgs), argNum) =>
              if (argNum < ParamRegisters.length) {
                usedParamRegisters += ((ParamRegisters(argNum), n))
              }
              Seq()

            case _: nodes.Block | _: nodes.Start | _: nodes.End | _: nodes.Proj |
                 _: nodes.Address | _: nodes.Const | _: nodes.Return | _: nodes.Jmp |
                 _: nodes.Cmp | _: nodes.Cond | _: nodes.Bad | _: nodes.Unknown => Seq()
          }
      }
    }

    private def getOperand(node: Node): Operand = getCanonicalNode(node) match {
      case n : nodes.Const => ConstOperand(n.getTarval.asInt(), n.getMode.getSizeBytes)
      case n @ ProjExtr(ProjExtr(_, nodes.Start.pnTArgs), argNum) =>
        if (argNum < ParamRegisters.length) regOp(n) else ActivationRecordOperand(
        8 * (argNum - ParamRegisters.length + 1 /* return address */), n.getMode.getSizeBytes)
      case n : nodes.Unknown => ConstOperand(0, n.getMode.getSizeBytes)
      case n => regOp(n)
    }

    private def getCanonicalNode(node: Node): Node = node match {
      case ProjExtr(ProjExtr(call: nodes.Call, nodes.Call.pnTResult), resultNo) =>
        assert(resultNo == 0)
        call
      case ProjExtr(load: nodes.Load, nodes.Load.pnRes) => load
      case ProjExtr(div: nodes.Div, nodes.Div.pnRes) => div
      case ProjExtr(mod: nodes.Mod, nodes.Mod.pnRes) => mod
      case _ => node
    }
  }

}
