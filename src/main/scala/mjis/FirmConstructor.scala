package mjis

import java.io.BufferedWriter

import firm.bindings.binding_ircons.op_pin_state
import firm.{ Program => FirmProgram, Ident => _, _ }
import firm.nodes._
import mjis.ast._
import mjis.util.FirmDumpHelper
import mjis.util.MapExtensions._
import scala.collection.JavaConversions._
import scala.collection.mutable

sealed abstract class ExprResult {}
// Result values of expression visitors are usually characterized by a single node representing the value...
final case class Value(node: Node) extends ExprResult
// ...boolean expressions, however, may alternatively be characterized by two list of jumps in order to minimize the
// number of control flow edges. Depending on the boolean value of the expression, exactly one jump node from the respective
// list will be activated upon evaluation.
// invariant for avoiding critical edges: each list of nodes contains exactly one node or two distinct nodes whose respective
// block has only one outgoing edge
final case class ControlFlow(falseJmps: List[Node], trueJmps: List[Node]) extends ExprResult

class FirmConstructor(input: Program) extends Phase[Unit] {
  private val firmClassEntity = new mutable.HashMap[ClassDecl, firm.Entity]()
  private val firmFieldEntity = new mutable.HashMap[FieldDecl, firm.Entity]()
  private val firmMethodEntity = new mutable.HashMap[MethodDecl, firm.Entity]()

  private val firmType = new mutable.HashMap[TypeDef, firm.Type]().withPersistentDefault {
    case Builtins.BooleanType => new PrimitiveType(Mode.getBu)
    case Builtins.IntType => new PrimitiveType(Mode.getIs)
    case Builtins.VoidType => throw new IllegalArgumentException("void doesn't have a runtime type")
    /* array and class types */
    case _ => new PrimitiveType(Mode.getP)
  }

  override protected def getResult(): Unit = transformProgram(input)

  override def dumpResult(writer: BufferedWriter): Unit = {
    FirmProgram.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-FirmConstructor"))
  }

  private def createMethodEntity(cls: ClassDecl, method: MethodDecl) = {
    val paramTypes: Array[Type] = (method.parameters map { p => firmType(p.typ) }).toArray
    val resultTypes: Array[Type] = method.returnType match {
      case Builtins.VoidType => Array[Type]()
      case t                 => Array[Type](firmType(t))
    }
    val methodType = new MethodType(paramTypes, resultTypes)
    new Entity(FirmProgram.getGlobalType, mangle(method, cls), methodType)
  }

  private def createClassEntity(cls: ClassDecl): Entity = {
    val struct = new StructType(cls.name)
    var offset = 0
    var align = 1
    for ((f, i) <- cls.fields.zipWithIndex) {
      val typ = firmType(f.typ)
      val firmField = Entity.createParameterEntity(struct, i, typ)
      firmFieldEntity += f -> firmField

      val fieldAlign = typ.getAlignmentBytes
      if (offset % fieldAlign > 0)
        offset += fieldAlign - offset % fieldAlign

      firmField.setOffset(offset)
      offset += typ.getSizeBytes

      align = math.max(align, fieldAlign) // powers of two
    }
    struct.setSizeBytes(offset)
    struct.setAlignmentBytes(align)
    struct.finishLayout()
    new Entity(firm.Program.getGlobalType, cls.name, struct)
  }

  private def transformProgram(program: Program): Unit = {
    // Create class, field and method entities
    program.classes.foreach(cls => {
      cls.methods.foreach(method => firmMethodEntity += method -> createMethodEntity(cls, method))
      firmClassEntity += cls -> createClassEntity(cls)
    })

    // Special case: System.out.println; it is the only MethodDecl in Builtins which is not an operator
    firmMethodEntity += Builtins.SystemOutPrintlnDecl -> createMethodEntity(null, Builtins.SystemOutPrintlnDecl)

    program.classes.foreach(_.methods.foreach(m => new MethodVisitor(m).visit(m)))
  }

  private def mangle(method: MethodDecl, cls: ClassDecl) = {
    if (method.isStatic) {
      if (method.name == "main")
        // we wrap the main method because of its missing return value
        "__main"
      else
        method.name.replace('.', '_')
    }
    else
      "_" + cls.name.length.toString + cls.name + "_" + method.name
  }

  private class MethodVisitor(method: MethodDecl) extends PlainRecursiveVisitor[Unit, Unit, ExprResult]((), (), null) {

    val graph = new Graph(firmMethodEntity(method), method.numVars)
    val constr = new Construction(graph)
    val declIndex: mutable.Map[Decl, Int] = new mutable.HashMap[Decl, Int]().withPersistentDefault(decl => declIndex.size)

    def firmArrayType(arrayType: TypeArray): ArrayType = {
      // For FIRM, only one dimension of an array is visible at a time.
      // Multidimensional arrays are just arrays of pointers.
      if (arrayType.numDimensions > 1) new ArrayType(new PrimitiveType(Mode.getP))
      else new ArrayType(firmType(arrayType.elementType))
    }

    def createAndMatureBlock(preds: Seq[Node]): firm.nodes.Block = {
      val block = constr.newBlock()
      preds.foreach(block.addPred)
      block.mature()
      block
    }

    def exprResultToValue(res: ExprResult): Value = res match {
      case v: Value => v
      case ControlFlow(falseJmps, trueJmps) =>
        constr.setCurrentBlock(createAndMatureBlock(falseJmps))
        val falseNode = constr.newConst(0, Mode.getBu)
        val leaveFalseJmp = constr.newJmp()

        constr.setCurrentBlock(createAndMatureBlock(trueJmps))
        val trueNode = constr.newConst(1, Mode.getBu)
        val leaveTrueJmp = constr.newJmp()

        constr.setCurrentBlock(createAndMatureBlock(Seq(leaveFalseJmp, leaveTrueJmp)))
        Value(constr.newPhi(Array(falseNode, trueNode), Mode.getBu))
    }

    def bToControlFlow(bNode: Node) = {
      val cond = constr.newCond(bNode)
      ControlFlow(List(constr.newProj(cond, Mode.getX, Cond.pnFalse)), List(constr.newProj(cond, Mode.getX, Cond.pnTrue)))
    }

    def exprResultToControlFlow(res: ExprResult): ControlFlow = res match {
      case cf: ControlFlow => cf
      case Value(node) => bToControlFlow(constr.newCmp(node, constr.newConst(1, Mode.getBu), Relation.Equal))
    }

    override def preVisit(method: MethodDecl) = {
      // Create a local variable for each parameter. Otherwise, parameters could not be set
      for ((param, index) <- method.parameters.zipWithIndex) {
        val proj = constr.newProj(graph.getArgs, firmType(param.typ).getMode, index)
        handleLocalVarAssignment(param, proj)
      }
    }

    override def postVisit(method: MethodDecl, _1: Unit, _2: Unit): Unit = {
      if (method.body.isEndReachable) {
        assert(method.returnType == Builtins.VoidType, "reachable end of non-void method")
        graph.getEndBlock.addPred(constr.newReturn(constr.getCurrentMem, Array[Node]()))
      }
      constr.finish()
      graph.check()
    }

    override def visit(stmt: mjis.ast.ExpressionStatement): Unit = {
      // If we don't call exprResultToValue, the control flow potentially
      // generated by the expression never gets wired up correctly
      exprResultToValue(stmt.expr.accept(this))
    }

    override def visit(stmt: mjis.ast.Block): Unit = {
      for (s <- stmt.statements) {
        s.accept(this)
        if (!s.isEndReachable)
          return // skip dead code
      }
    }

    override def visit(stmt: If): Unit = {
      val cond = exprResultToControlFlow(stmt.condition.accept(this))

      val follow = constr.newBlock()

      constr.setCurrentBlock(createAndMatureBlock(cond.falseJmps))
      stmt.ifFalse.accept(this)
      if (stmt.ifFalse.isEndReachable)
        follow.addPred(constr.newJmp())

      constr.setCurrentBlock(createAndMatureBlock(cond.trueJmps))
      stmt.ifTrue.accept(this)
      if (stmt.ifTrue.isEndReachable)
        follow.addPred(constr.newJmp())

      constr.setCurrentBlock(follow)
      follow.mature()
    }

    override def visit(stmt: While): Unit = {
      val jmp = constr.newJmp()

      val condBlock = constr.newBlock()
      graph.keepAlive(condBlock)
      constr.setCurrentBlock(condBlock)
      condBlock.addPred(jmp)
      val cond = exprResultToControlFlow(stmt.condition.accept(this))

      constr.setCurrentBlock(createAndMatureBlock(cond.trueJmps))
      stmt.body.accept(this)
      if (stmt.body.isEndReachable)
        condBlock.addPred(constr.newJmp())

      condBlock.mature()

      constr.setCurrentBlock(createAndMatureBlock(cond.falseJmps))
    }

    override def postVisit(stmt: ReturnStatement, optExprResult: Option[ExprResult]): Unit = {
      // evaluate optExprResult first since it could introduce new blocks
      val returnValues = optExprResult match {
        case None             => Array[Node]()
        case Some(exprResult) => Array[Node](exprResultToValue(exprResult).node)
      }
      val returnNode = constr.newReturn(constr.getCurrentMem, returnValues)
      graph.getEndBlock.addPred(returnNode)
    }

    private def handleLocalVarAssignment(decl: TypedDecl, rhs: Node): Node = {
      val index = declIndex(decl)
      constr.setVariable(index, rhs)
      constr.getVariable(index, firmType(decl.typ).getMode)
    }

    override def postVisit(stmt: LocalVarDeclStatement, typResult: Unit, initializerResult: Option[ExprResult]): Unit =
      initializerResult.foreach(init => handleLocalVarAssignment(stmt, exprResultToValue(init).node))

    override def postVisit(expr: BooleanLiteral): ExprResult = {
      Value(constr.newConst(expr match {
        case TrueLiteral()  => 1
        case FalseLiteral() => 0
      }, Mode.getBu))
    }

    override def postVisit(expr: IntLiteral): ExprResult = {
      Value(Typer.getType(expr) match {
        case Builtins.IntType => constr.newConst(expr.value.toInt, Mode.getIs)
        case _ => throw new IllegalArgumentException("Invalid type for IntLiteral")
      })
    }

    override def visit(expr: Apply): ExprResult = {
      expr.decl match {
        // short-circuiting operators

        case Builtins.BooleanNotDecl =>
          val lhs = exprResultToControlFlow(expr.arguments(0).accept(this))
          ControlFlow(lhs.trueJmps, lhs.falseJmps)
        case Builtins.BooleanAndDecl =>
          val lhs = exprResultToControlFlow(expr.arguments(0).accept(this))

          constr.setCurrentBlock(createAndMatureBlock(lhs.trueJmps))
          val rhs = exprResultToControlFlow(expr.arguments(1).accept(this))

          // collectJmps necessary to uphold ControlFlow invariant
          ControlFlow(lhs.falseJmps ++ rhs.falseJmps, rhs.trueJmps)
        case Builtins.BooleanOrDecl =>
          val lhs = exprResultToControlFlow(expr.arguments(0).accept(this))

          constr.setCurrentBlock(createAndMatureBlock(lhs.falseJmps))
          val rhs = exprResultToControlFlow(expr.arguments(1).accept(this))

          // collectJmps necessary to uphold ControlFlow invariant
          ControlFlow(rhs.falseJmps, lhs.trueJmps ++ rhs.trueJmps)

        // other methods

        case _ =>
          // Fully process one argument after another, else stray blocks without successors might occur.
          val args = expr.arguments.map(arg => exprResultToValue(arg.accept(this)).node)
          def cmp(r: Relation) = bToControlFlow(constr.newCmp(args(0), args(1), r))

          expr.decl match {
            // int comparisons (Value -> Value -> ControlFlow)

            case Builtins.IntLessDecl => cmp(Relation.Less)
            case Builtins.IntLessEqualDecl => cmp(Relation.LessEqual)
            case Builtins.IntGreaterDecl => cmp(Relation.Greater)
            case Builtins.IntGreaterEqualDecl => cmp(Relation.GreaterEqual)
            case Builtins.EqualsDecl => cmp(Relation.Equal)
            case Builtins.UnequalDecl => cmp(Relation.UnorderedLessGreater)

            // other (Value-based) methods

            case _ => Value(expr.decl match {
              case Builtins.IntAddDecl => constr.newAdd(args(0), args(1), Mode.getIs)
              case Builtins.IntSubDecl => constr.newSub(args(0), args(1), Mode.getIs)
              case Builtins.IntMulDecl => constr.newMul(args(0), args(1), Mode.getIs)
              case Builtins.IntDivDecl =>
                val divNode = constr.newDiv(constr.getCurrentMem, args(0), args(1),
                  Mode.getIs, op_pin_state.op_pin_state_pinned)
                constr.setCurrentMem(constr.newProj(divNode, Mode.getM, firm.nodes.Div.pnM))
                constr.newProj(divNode, Mode.getIs, firm.nodes.Div.pnRes)
              case Builtins.IntModDecl =>
                val modNode = constr.newMod(constr.getCurrentMem, args(0), args(1),
                  Mode.getIs, op_pin_state.op_pin_state_pinned)
                constr.setCurrentMem(constr.newProj(modNode, Mode.getM, firm.nodes.Mod.pnM))
                constr.newProj(modNode, Mode.getIs, firm.nodes.Mod.pnRes)

              case Builtins.IntMinusDecl =>
                constr.newMinus(args(0), args(0).getMode)

              case Builtins.ArrayAccessDecl =>
                val sel = createArrayAccess(expr, args)
                if (expr.isLvalue)
                  sel
                else {
                  val arrayType = Typer.getType(expr.arguments(0)).asInstanceOf[TypeArray]
                  val resultType = firmArrayType(arrayType).getElementType
                  createLoad(sel, resultType)
                }

              case decl => call(firmMethodEntity(decl), args.toArray)
            })
          }
      }
    }

    private def createLoad(ptr: Node, typ: Type): Node = {
      // FIRM doesn't have a proper concept for array loads, so we need to catch that case and special case it
      val mode = if (typ.isInstanceOf[ArrayType]) Mode.getP else typ.getMode
      val load = constr.newLoad(constr.getCurrentMem, ptr, mode, typ)
      constr.setCurrentMem(constr.newProj(load, Mode.getM, firm.nodes.Load.pnM))
      constr.newProj(load, mode, firm.nodes.Load.pnRes)
    }

    private def createStore(ptr: Node, value: Node): Node = {
      val store = constr.newStore(constr.getCurrentMem, ptr, value)
      val newMem = constr.newProj(store, Mode.getM, firm.nodes.Store.pnM)
      constr.setCurrentMem(newMem)
      value
    }

    override def postVisit(expr: Select, qualifierResult: ExprResult): ExprResult = {
      val qualifier = exprResultToValue(qualifierResult).node
      assert(qualifier.getMode == Mode.getP, qualifier.getMode)
      val fieldEntity = firmFieldEntity(expr.decl)
      val member = constr.newMember(qualifier, fieldEntity)
      Value(
        if (expr.isLvalue) member
        else createLoad(member, fieldEntity.getType)
      )
    }

    private val callocType = {
      val Is = firmType(Builtins.IntType)
      val params: Array[Type] = Array(Is, Is)
      // we can't construct a void*, so we take the next best thing: char*
      val result: Array[Type] = Array(new PointerType(new PrimitiveType(Mode.getBu)))
      new MethodType(params, result)
    }
    private val calloc = new Entity(FirmProgram.getGlobalType, "calloc", callocType)

    private def call(methodEntity: Entity, args: Array[Node]): Node = {
      val methodType = methodEntity.getType.asInstanceOf[MethodType]
      val addr = constr.newAddress(methodEntity)
      val call = constr.newCall(constr.getCurrentMem, addr, args, methodEntity.getType)
      constr.setCurrentMem(constr.newProj(call, Mode.getM, firm.nodes.Call.pnM))
      // libFirm *really* doesn't like us trying to access void
      if (methodType.getNRess > 0) {
        val res = constr.newProj(call, Mode.getT, firm.nodes.Call.pnTResult)
        constr.newProj(
          res,
          methodType.getResType(0).getMode,
          0)
      } else call
    }

    override def postVisit(expr: NewArray, _1: Unit, firstDimSize: ExprResult): ExprResult = {
      val arrayType = Typer.getType(expr).asInstanceOf[TypeArray]
      val baseType = firmArrayType(arrayType).getElementType

      // evaluate elems first since it could introduce new blocks
      val elems = exprResultToValue(firstDimSize).node
      val size = constr.newConst(baseType.getSizeBytes, Mode.getIs)
      Value(call(calloc, Array[Node](elems, size)))
    }

    override def postVisit(expr: NewObject, _1: Unit): ExprResult = {
      val classType = firmClassEntity(expr.typ.decl).getType
      val size = constr.newConst(1 max classType.getSizeBytes, Mode.getIs)
      val num_elems = constr.newConst(1, Mode.getIs)
      Value(call(calloc, Array[Node](num_elems, size)))
    }

    private def createArrayAccess(expr: Apply, args: List[Node]): Node = {
      assert(expr.decl eq Builtins.ArrayAccessDecl, "method calls may not be used as lvalues")
      val arrayType = Typer.getType(expr.arguments(0)).asInstanceOf[TypeArray]
      constr.newSel(args(0), args(1), firmArrayType(arrayType))
    }

    override def postVisit(expr: Assignment, lhsResult: ExprResult, rhsResult: ExprResult): ExprResult = {
      // evaluate rhs first since it could introduce new blocks
      val rhs = exprResultToValue(rhsResult).node
      val lhs = lhsResult.asInstanceOf[Value].node // L-values are always Values

      Value(expr.lhs match {
        case ident: Ident => ident.decl match {
          case field: FieldDecl =>
            createStore(lhs, rhs)
          case local@(_: Parameter | _: LocalVarDeclStatement) =>
            handleLocalVarAssignment(local, rhs)
        }
        case sel: Select =>
          createStore(lhs, rhs)
        case arrAccess: Apply =>
          assert(arrAccess.decl eq Builtins.ArrayAccessDecl, "method calls are not valid lvalues")
          createStore(lhs, rhs)
        case other => throw new UnsupportedOperationException(s"Unexpected LHS value: $other")
      })
    }

    override def postVisit(ident: Ident): ExprResult = Value(ident.decl match {
      case local @ (_: Parameter | _: LocalVarDeclStatement) =>
        constr.getVariable(declIndex(local), firmType(local.asInstanceOf[TypedDecl].typ).getMode)
      case field: FieldDecl =>
        val fieldEntity = firmFieldEntity(field)
        val thisPtr = constr.getVariable(0, Mode.getP)
        val member = constr.newMember(thisPtr, fieldEntity)
        if (ident.isLvalue)
          member
        else
          createLoad(member, fieldEntity.getType)
    })

    override def postVisit(thisLit: ThisLiteral): ExprResult =
      Value(constr.getVariable(declIndex(thisLit.decl), firmType(thisLit.decl.typ).getMode))

    override def postVisit(nullLiteral: NullLiteral): ExprResult = Value(constr.newConst(0, Mode.getP))
  }

  override def findings: List[Finding] = List()
}
