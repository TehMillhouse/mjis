package mjis

import java.io._

import firm._
import firm.nodes.{Bad, Block, Node}
import mjis.asm._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.util.MapExtensions._
import mjis.asm.AMD64Registers._
import mjis.util.{Digraph, PowerOfTwo}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

object CodeGenerator {
  def align(x: Int, alignment: Int = 8) = if (x % alignment == 0) x else x + (alignment - x % alignment)

  var uniqueLabelNr = 0
  def newTempLabelOp() = {
    val labelNr = uniqueLabelNr
    uniqueLabelNr += 1
    LabelOperand(s".T$labelNr")
  }
}

class CodeGenerator(a: Unit) extends Phase[AsmProgram] {
  var onlyReachableFromMain: Boolean = true

  override def dumpResult(a: BufferedWriter) = {
    a.write(new MjisAssemblerFileGenerator(result, null).generateCode())
  }

  override def getResult(): AsmProgram = {
    val functions = Program.getGraphs.map(g => g -> new MethodCodeGenerator(g).getResult()).toMap
    val callEdges = functions.map {
      case (graph, fun) => fun -> CallGraph.calls(graph).flatMap(_.getCalledGraph).flatMap(functions.get)
    }.toMap

    val callGraph = new Digraph[AsmFunction](callEdges)
    val mainFunction = functions.values.find(_.name == "__main").get

    new AsmProgram(
      if (onlyReachableFromMain) callGraph.getTopologicalSorting(mainFunction) else functions.values.toSeq,
      new Digraph[AsmFunction](callEdges))
  }

  def intConstOp(i: Int): Operand = ConstOperand(i, Mode.getIs.getSizeBytes)

  class MethodCodeGenerator(g: Graph) {
    val basicBlocks = mutable.ListMap[Block, AsmBasicBlock]().
      withPersistentDefault(b => new AsmBasicBlock(if (b == null) -1 else b.getNr))
    val function = new AsmFunction(g.getEntity.getLdName)

    val usedParams = mutable.Map[Int, Node]()
    val toVisit = mutable.Queue[Node]()

    val inductionVarAdds = g.getInductionVariables.map(_.incrAdd).toSet[Node]

    def regOp(node: Node, nr: Int = 0): RegisterOperand = RegisterOperand(node.idx + nr * (1 + g.getLastIdx), node match {
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

    def getResult(): AsmFunction = {
      val backEdgesWereEnabled = BackEdges.enabled(g)
      BackEdges.enable(g)

      val instructions = mutable.Map[Node, Seq[Instruction]]()

      g.walkTopologicalWith { n =>
        val isBlockRoot = BackEdges.getOuts(n).exists(e => e.node.block != null && e.node.block != n.block)
        if (isBlockRoot)
          instructions(n) = createValue(n)
      }
      while (toVisit.nonEmpty) {
        val n = toVisit.dequeue()
        if (!instructions.contains(n))
          instructions(n) = createValue(n)
      }

      val firmBlocks = linearizeBlocks().map(basicBlocks).toList

      firmBlocks.head.predecessors += Some(function.prologue)
      function.prologue.successors += firmBlocks.head
      function.epilogue = basicBlocks(g.getEndBlock)

      function.basicBlocks = function.prologue :: firmBlocks

      for ((firmBlock, asmBlock) <- basicBlocks) {
        asmBlock.predecessors ++= firmBlock.getPreds.map(b =>
          if (b.isInstanceOf[Bad] || !function.basicBlocks.contains(basicBlocks(b.block))) None
          else Some(basicBlocks(b.block)))
      }

      val nextBlock = function.basicBlocks.zip(function.basicBlocks.tail).toMap

      val yoloScheduling = mutable.HashMap[AsmBasicBlock, Seq[Instruction]]().withPersistentDefault(_ => Seq())

      g.walkTopologicalWith { n =>
        val basicBlock = basicBlocks(n.block)
        var instrs = instructions.getOrElse(n, Seq()) map (_.withComment(s" - $n"))

        // delay scheduling of single-use induction variable adds in order to force them into a single register
        if (inductionVarAdds(n) && BackEdges.getNOuts(n) == 1)
          yoloScheduling(basicBlock) ++= instrs
        else
          basicBlock.instructions ++= instrs
      }

      basicBlocks.values.foreach(b => b.instructions ++= yoloScheduling(b))

      g.walkTopologicalWith { n =>
        val basicBlock = basicBlocks(n.block)
        basicBlock.instructions ++= createControlFlow(n, nextBlock.get(basicBlock)) map (_.withComment(s" - $n"))
      }
      if (!backEdgesWereEnabled) BackEdges.disable(g)

      for ((argNum, n) <- usedParams.toSeq.sortBy(_._1))
        // save used param registers, load used stack params
        function.prologue.instructions += Mov(
          if (argNum < ParamRegisters.length) RegisterOperand(ParamRegisters(argNum), n.getMode.getSizeBytes)
          else ActivationRecordOperand(8 * (argNum - ParamRegisters.length + 1 /* return address */), n.getMode.getSizeBytes),
          regOp(n))

      function.epilogue.instructions +=
        (if (g.methodType.getNRess > 0) Ret(g.methodType.getResType(0).getSizeBytes) else Ret())
      function
    }

    private def linearizeBlocks(): Seq[Block] = {
      // keep blocks of a loop together by recursively using a topological ordering of the condensation graphs
      g.getBlockGraph.transposed.getSCCTree(g.getStartBlock).flatMap(_.nodes)
    }

    private def createControlFlow(node: Node, nextBlock: Option[AsmBasicBlock]): Seq[Instruction] = {
      def successorBlock(node: Node) = BackEdges.getOuts(node).iterator().next().node.asInstanceOf[Block]

      node match {
        case _: nodes.Jmp =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          Seq()

        case ReturnExtr(retvalOption) =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          retvalOption match {
            case Some(retval) => Seq(Mov(getOperand(retval), RegisterOperand(RAX, g.methodType.getResType(0).getSizeBytes)))
            case None => Seq()
          }

        case CondExtr(cmp: nodes.Cmp) =>
          val successors = BackEdges.getOuts(node).map(_.node.asInstanceOf[nodes.Proj]).toList
          val projTrue = successors.find(_.getNum == nodes.Cond.pnTrue)
          val projFalse = successors.find(_.getNum == nodes.Cond.pnFalse)
          assert(projTrue.isDefined && projFalse.isDefined)

          basicBlocks(node.block).successors += basicBlocks(successorBlock(projTrue.get))
          basicBlocks(node.block).successors += basicBlocks(successorBlock(projFalse.get))

          basicBlocks(node.block).relation = cmp.getRelation
          Seq(asm.Cmp(getOperand(cmp.getLeft), getOperand(cmp.getRight)).withComment(s"Evaluate $cmp"))

        case _ => Seq()
      }
    }

    private def getDivModCode(n: Node, left: Node, right: Node): Seq[Instruction] = {
      assert(left.getMode == Mode.getIs)
      assert(right.getMode == Mode.getIs)
      val rightOp = getOperand(right)
      assert(!rightOp.isInstanceOf[ConstOperand])
      Seq(
        Mov(getOperand(left), RegisterOperand(RAX, 4)),
        Cdq(4) /* sign-extend eax into edx:eax */,
        IDiv(rightOp)
      )
    }

    private def createValue(node: Node): Seq[Instruction] = {
      def getAddressOperand(node: Node, sizeBytes: Int): AddressOperand = node match {
        case n: nodes.Member =>
          toVisit += n.getPtr
          AddressOperand(
            base = Some(getOperand(n.getPtr).asInstanceOf[RegisterOperand]),
            offset = n.getEntity.getOffset,
            sizeBytes = sizeBytes)
        case n@SelExtr(base, i@ConstExtr(c)) =>
          val opBase = base match {
            case _: nodes.Const => None
            case _ => Some(getOperand(base, forceRegister = true).asInstanceOf[RegisterOperand])
          }
          toVisit += base
          val elemSize = n.asInstanceOf[nodes.Sel].getType.asInstanceOf[firm.ArrayType].getElementType.getSizeBytes
          try {
            AddressOperand(
              base = opBase,
              offset = Math.multiplyExact(c, elemSize),
              sizeBytes = sizeBytes)
          } catch {
            case _: ArithmeticException => // offset calculation overflows, we can't encode that as an offset
              AddressOperand(
                base = opBase,
                indexAndScale = Some((regOp(i), elemSize)),
                sizeBytes = sizeBytes)
          }
        case n@SelExtr(base, index) =>
          val elemSize = n.asInstanceOf[nodes.Sel].getType.asInstanceOf[firm.ArrayType].getElementType.getSizeBytes
          val opBase = base match {
            case _: nodes.Const => None
            case _ => Some(getOperand(base, forceRegister = true).asInstanceOf[RegisterOperand])
          }
          toVisit ++= Seq(base, index)
          AddressOperand(
            base = opBase,
            indexAndScale = Some((getOperand(index, forceRegister = true).asInstanceOf[RegisterOperand], elemSize)),
            sizeBytes = sizeBytes)
        case AddExtr(base, ConstExtr(offset)) =>
          toVisit += base
          AddressOperand(
            base = Some(getOperand(base).asInstanceOf[RegisterOperand]),
            offset = offset,
            sizeBytes = sizeBytes)
        case AddExtr(
          GenAddExtr(base, offset),
          GenMulExtr(
            Some(GenConvExtr(index)),
            scale@(1 | 2 | 4 | 8)
          )
        ) =>
          toVisit ++= Seq(base, Some(index)).flatten
          AddressOperand(
            base = base.map(getOperand(_).asInstanceOf[RegisterOperand]),
            indexAndScale = Some((
              getOperand(index).asInstanceOf[RegisterOperand],
              scale)),
            offset = offset,
            sizeBytes = sizeBytes)
        case AddExtr(base, index) =>
          toVisit ++= Seq(base, index)
          AddressOperand(
            base = Some(getOperand(base).asInstanceOf[RegisterOperand]),
            indexAndScale = Some((getOperand(index).asInstanceOf[RegisterOperand], 1)),
            sizeBytes = sizeBytes)
        case GenConvExtr(MulExtr(
          GenConvExtr(index),
          ConstExtr(scale@(1 | 2 | 4 | 8))
        )) =>
          toVisit += index
          AddressOperand(
            indexAndScale = Some((
              getOperand(index).asInstanceOf[RegisterOperand],
              scale)),
            sizeBytes = sizeBytes)
        case GenConvExtr(other) =>
          toVisit += other
          AddressOperand(
            base = Some(getOperand(other).asInstanceOf[RegisterOperand]),
            sizeBytes = sizeBytes)
      }

      def divByConstant(dividend: Node, divisor: Int): Seq[Instruction] = {
        // See http://support.amd.com/TechDocs/25112.PDF pages 189 and following
        def log2(i: Long) = {
          assert(i > 0)
          def rec(i: Long, acc: Int): Int = i match {
            case 0 => acc
            case _ => rec(i >> 1, acc + 1)
          }
          rec(i >> 1, 0)
        }

        assert(dividend.getMode == Mode.getIs)
        val r = regOp(node)
        val eax = RegisterOperand(AMD64Registers.RAX, 4)
        val edx = RegisterOperand(AMD64Registers.RDX, 4)
        val (instrs: Seq[Instruction], resultReg) = Math.abs(divisor.toLong) match {
          case 0 => (Seq(
            Mov(getOperand(dividend), eax),
            Mov(ConstOperand(0, 4), r),
            IDiv(r)
          ), eax)
          case 1 => ??? // handled by Identities
          case 2 => (Seq(
            Mov(getOperand(dividend), r),
            Cmp(r, ConstOperand(1 << 31, 4)),
            new Instruction("sbb", (ConstOperand(-1, 4), OperandSpec.READ), (r, OperandSpec.READ | OperandSpec.WRITE)),
            Sar(ConstOperand(1, 4), r)
          ), r)
          case d@PowerOfTwo(pow) => (Seq(
            Mov(getOperand(dividend), eax),
            Cdq(4),
            And(ConstOperand((d - 1).toInt, 4), edx),
            Add(edx, eax),
            Sar(ConstOperand(pow, 4), eax)
          ), eax)
          case d =>
            /* Determine algorithm (a), multiplier (m), and shift factor (s) for 32-bit
            signed integer division. Based on: Granlund, T.; Montgomery, P.L.:
            "Division by Invariant Integers using Multiplication". SIGPLAN Notices,
            Vol. 29, June 1994, page 61.
            */
            var l = log2(d)
            val j = (1l << 31) % d
            val k = (1l << (32 + l)) / ((1l << 31) - j)
            var m_low = (1l << (32 + l)) / d
            var m_high = ((1l << (32 + l)) + k) / d
            while (((m_low >> 1) < (m_high >> 1)) && (l > 0)) {
              m_low >>= 1
              m_high >>= 1
              l -= 1
            }
            val m = m_high.toInt
            (Seq(
              Mov(ConstOperand(m, 4), eax),
              IMul(getOperand(dividend)),
              Mov(getOperand(dividend), eax)) ++
              (if ((m_high >> 31) != 0) Seq(Add(eax, edx)) else Seq()) ++ Seq(
              Sar(ConstOperand(l, 4), edx),
              Shr(ConstOperand(31, 4), eax),
              Add(eax, edx)
            ), edx)
        }
        instrs ++ (if (divisor < 0) Seq(Neg(resultReg)) else Seq()) :+ Mov(resultReg, r)
      }

      node match {
        // special tree patterns that don't necessarily visit all predecessors

        case n: nodes.Add => Seq(Lea(getAddressOperand(n, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Sel =>
          // This only happens in the case of Loop Strength Reduction, which inserts Pointer Phis.
          Seq(Lea(getAddressOperand(n, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Load =>
          toVisit ++= Seq(n.getMem)
          n.getPtr match {
            case MemberExtr(_: nodes.Unknown, _)
                 | SelExtr(_: nodes.Unknown, _)
                 | MemberExtr(_: nodes.Const, _)
                 | SelExtr(_: nodes.Const, _) =>
              val ptr = n.getPtr match {
                // this will technically generate wrong code, i.e. null accesses instead of member accesses to null objects
                // but that's a very "well, actually" kind of problem
                case member: nodes.Member => member.getPtr
                case sel: nodes.Sel => sel.getPtr
                case _ => n.getPtr
              }
              Seq(
                Mov(getOperand(ptr), regOp(ptr)),
                Mov(AddressOperand(base = Some(regOp(ptr)), sizeBytes = n.getLoadMode.getSizeBytes), regOp(n)))
            case _ => Seq(Mov(getAddressOperand(n.getPtr, n.getLoadMode.getSizeBytes), regOp(n)))
          }
        case n: nodes.Store =>
          toVisit ++= Seq(n.getMem, n.getValue)
          n.getPtr match {
            case MemberExtr(_: nodes.Unknown, _)
                 | SelExtr(_: nodes.Unknown, _)
                 | MemberExtr(_: nodes.Const, _)
                 | SelExtr(_: nodes.Const, _) =>
              // null or undefined access
              val ptr = n.getPtr match {
                // see above
                case member: nodes.Member => member.getPtr
                case sel: nodes.Sel => sel.getPtr
                case _ => n.getPtr
              }
              Seq(
                Mov(getOperand(ptr), regOp(ptr)),
                Mov(getOperand(n.getValue), AddressOperand(base = Some(regOp(ptr)), sizeBytes = n.getType.getSizeBytes)))
            case _ => Seq(Mov(getOperand(n.getValue), getAddressOperand(n.getPtr, n.getType.getSizeBytes)))
          }

        case n: nodes.Mux =>
          toVisit ++= Seq(n.getFalse, n.getTrue) ++ n.getSel.getPreds // don't visit the Cmp node itself
          val cmp = n.getSel.asInstanceOf[nodes.Cmp]
          val cmpLeft = getOperand(cmp.getLeft)
          val cmpRight = getOperand(cmp.getRight)
          val ifFalse = getOperand(n.getFalse)
          val ifTrue = getOperand(n.getTrue)

          // Conditional move does not take immediates, so try to make trueOperand a non-constant
          // if it happens to be equal to the left operand of the comparison.
          val (relation, trueOperand, falseOperand) = (cmp.getRelation, cmpRight, ifTrue, ifFalse) match {
            // y = if (left == const) const else b  ==>  y = if (left == const) left else b
            case (Relation.Equal, _: ConstOperand, `cmpRight`, _) => (cmp.getRelation, cmpLeft, ifFalse)
            // y = if (left != const) b else const  ==>  y = if (left == const) left else b
            case (Relation.UnorderedLessGreater, _: ConstOperand, _, `cmpRight`) => (cmp.getRelation.negated(), cmpLeft, ifTrue)
            case _ => (cmp.getRelation, ifTrue, ifFalse)
          }

          trueOperand match {
            case _: ConstOperand => Seq(
              Mov(trueOperand, regOp(n, 1)).withComment("Load cmov argument"),
              Mov(falseOperand, regOp(n)),
              asm.Cmp(cmpLeft, cmpRight).withComment(s"Evaluate $cmp"),
              MovConditional(regOp(n, 1), regOp(n), relation)
            )
            case _ => Seq(
              Mov(falseOperand, regOp(n)),
              asm.Cmp(cmpLeft, cmpRight).withComment(s"Evaluate $cmp"),
              MovConditional(trueOperand, regOp(n), relation)
            )
          }

        case _ =>
          // non-special patterns
          toVisit ++= node.getPreds
          node match {
            case n: nodes.And => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.And(getOperand(n.getRight), regOp(n)))
            case n: nodes.Sub => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.Sub(getOperand(n.getRight), regOp(n)))
            case n: nodes.Minus => Seq(Mov(getOperand(n.getOp), regOp(n)), Neg(regOp(n)))
            case n: nodes.Not => Seq(Mov(getOperand(n.getOp), regOp(n)), Not(regOp(n)))

            case n@MulExtr(x, c@ConstExtr(PowerOfTwo(shift))) =>
              Seq(Mov(getOperand(x), regOp(n)),
              Shl(ConstOperand(shift, c.getMode.getSizeBytes), regOp(n)))

            case n : nodes.Mul =>
              val tempRegister = RegisterOperand(RAX, n.getMode.getSizeBytes)
              // Normalization moves constants to the right of a Mul node,
              // but the Mul instruction cannot take a constant as operand.
              // Avoid having to allocate an extra register by swapping left and right.
              Seq(Mov(getOperand(n.getRight), tempRegister), asm.Mul(getOperand(n.getLeft)),
                Mov(tempRegister, regOp(n)))

            case DivExtr(dividend, ConstExtr(divisor)) => divByConstant(dividend, divisor)
            case n : firm.nodes.Div => getDivModCode(n, n.getLeft, n.getRight) ++ Seq(Mov(RegisterOperand(RAX, 4), regOp(n)))
            case n : firm.nodes.Mod => getDivModCode(n, n.getLeft, n.getRight) ++ Seq(Mov(RegisterOperand(RDX, 4), regOp(n)))

            case n@CallExtr(address, params) =>
              val resultInstrs = ListBuffer[Instruction]()

              val (regParams, stackParams) = params.splitAt(ParamRegisters.length)
              val regSrcOperands = regParams.map(getOperand(_))
              val paramRegisters = regSrcOperands.zip(ParamRegisters).map {
                case (srcOp, reg) => RegisterOperand(reg, srcOp.sizeBytes)
              }

              for ((srcOp, destOp) <- regSrcOperands zip paramRegisters) {
                resultInstrs += Mov(srcOp, destOp).withComment(s"Load register argument")
              }

              val stackPointerOffset = 8 * stackParams.length
              if (stackParams.nonEmpty) {
                resultInstrs += Sub(intConstOp(stackPointerOffset), RegisterOperand(RSP, 8)).withComment(s"Move stack pointer for parameters")
                // Push rest of parameters onto the stack in reverse order
                for ((param, i) <- stackParams.zipWithIndex.reverse) {
                  val srcOp = getOperand(param)
                  val tempOp = RegisterOperand(n.idx, srcOp.sizeBytes) // needed if srcOp is a memory operand
                  resultInstrs += Mov(srcOp, tempOp).
                    withComment(s"Reload stack argument").
                    withStackPointerOffset(stackPointerOffset)
                  resultInstrs += Mov(tempOp, AddressOperand(
                    base = Some(RegisterOperand(RSP, 8)),
                    offset = 8 * i,
                    sizeBytes = srcOp.sizeBytes)).
                    withComment(s"Push stack argument").
                    withStackPointerOffset(stackPointerOffset)
                }
              }

              val methodName = address.getEntity.getLdName
              val methodType = address.getEntity.getType.asInstanceOf[MethodType]
              val returnsValue = methodType.getNRess > 0

              resultInstrs +=
                (if (returnsValue) mjis.asm.Call(LabelOperand(methodName), methodType.getResType(0).getSizeBytes, paramRegisters)
                else mjis.asm.Call(LabelOperand(methodName), paramRegisters))

              if (stackParams.nonEmpty)
                resultInstrs += Add(intConstOp(stackPointerOffset), RegisterOperand(RSP, 8)).withComment(s"Restore stack pointer")

              if (returnsValue) {
                resultInstrs += Mov(RegisterOperand(RAX, methodType.getResType(0).getSizeBytes), regOp(n))
              }

              resultInstrs

            case n: nodes.Phi =>
              if (n.getMode != Mode.getM && n.getMode != Mode.getX)
                basicBlocks(n.block).phis += Phi(n.getPreds.map(getOperand(_)).toSeq, regOp(n))
              Seq()

            case _: nodes.Block | _: nodes.Start | _: nodes.End | _: nodes.Proj |
                 _: nodes.Address | _: nodes.Const | _: nodes.Return | _: nodes.Jmp |
                 _: nodes.Cmp | _: nodes.Cond | _: nodes.Bad | _: nodes.Unknown |
                 _: nodes.Member => Seq()
          }
      }
    }

    private def getOperand(node: Node, forceRegister: Boolean = false): Operand =
      getCanonicalNode(node) match {
      case n : nodes.Const => if (forceRegister) regOp(n) else ConstOperand(n.getTarval.asInt(), n.getMode.getSizeBytes)
      case n @ ProjExtr(ProjExtr(_, nodes.Start.pnTArgs), argNum) =>
        usedParams(argNum) = n
        regOp(n)
      case n : nodes.Unknown => if (forceRegister) regOp(n) else ConstOperand(0, n.getMode.getSizeBytes)
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
