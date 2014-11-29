package mjis

import java.io.BufferedWriter

import firm.bindings.binding_ircons.op_pin_state
import firm.{Program => P, _}
import firm.nodes._
import mjis.ast._
import scala.collection.JavaConversions._
import scala.collection.mutable

class FirmConstructor(input: Program) extends Phase[Unit] {
  override protected def getResult(): Unit = {
    transformProgram(input)
  }

  override def dumpResult(writer: BufferedWriter): Unit = {
    P.getGraphs.foreach(Dump.dumpGraph(_, "-FirmConstructor"))
  }

  private def transformProgram(prog: Program) = new FirmConstructorVisitor().visit(prog)

  private def mangle(methodName: String) = methodName.replace('.', '_') // TODO

  private class FirmConstructorVisitor extends PlainRecursiveVisitor[Unit, Node, Node]((), null, null) {

    private var graph: Graph = null
    private var constr: Construction = null

    val firmFieldEntity = new mutable.HashMap[FieldDecl, firm.Entity]()
    val firmClassEntity = new mutable.HashMap[ClassDecl, firm.Entity]() {
      override def default(cls: ClassDecl): firm.Entity = {
        val struct = new StructType(cls.name)
        var offset = 0
        var align = 1
        for ((f, i) <- cls.fields.zipWithIndex) {
          val typ = firmType(f.typ)
          val firmField = Entity.createParameterEntity(struct, i, typ)

          // TODO: Align type 'b' like type 'Bu'
          val fieldAlign = typ.getAlignmentBytes
          if (offset % fieldAlign > 0)
            offset += fieldAlign  - offset % fieldAlign

          firmField.setOffset(offset)
          offset += typ.getSizeBytes

          align = math.max(align, fieldAlign) // powers of two
        }
        struct.setSizeBytes(offset)
        struct.setAlignmentBytes(align)
        struct.finishLayout()
        new Entity(firm.Program.getGlobalType, cls.name, struct)
      }
    }
    val firmType: mutable.Map[TypeDef, firm.Type] = new mutable.HashMap[TypeDef, firm.Type]() {
      override def default(typ: TypeDef): firm.Type = typ match {
        case Builtins.BooleanType => new PrimitiveType(Mode.getb)
        case Builtins.IntType => new PrimitiveType(Mode.getIs)
        case Builtins.ExtendedIntType => new PrimitiveType(Mode.getIu)
        case Builtins.VoidType => throw new IllegalArgumentException("void doesn't have a runtime type")
        case _ => new PrimitiveType(Mode.getP)
      }
    }
    private val methodEntity = new mutable.HashMap[MethodDecl, Entity] {
      override def default(decl: MethodDecl) = {
        val paramTypes: Array[Type] = (decl.parameters map { p => firmType(p.typ) }).toArray
        val resultTypes: Array[Type] = decl.typ match {
          case Builtins.VoidType => Array[Type]()
          case t => Array[Type](firmType(t))
        }
        val methodType = new MethodType(paramTypes, resultTypes)
        new Entity(P.getGlobalType, mangle(decl.name), methodType)
      }
    }

    override def preVisit(method: MethodDecl) = {
      graph = new Graph(methodEntity(method), method.numVars)
      constr = new Construction(graph)
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node): Unit = {
      if (method.typ == Builtins.VoidType) {
        graph.getEndBlock.addPred(constr.newReturn(constr.getCurrentMem, Array[Node]()))
      }
      constr.finish()
    }

    override def postVisit(stmt: ReturnStatement, exprResult: Option[Node]): Node = {
      val returnNode = constr.newReturn(constr.getCurrentMem, exprResult match {
        case None => Array[Node]()
        case Some(valueNode) => Array[Node](valueNode)
      })
      graph.getEndBlock.addPred(returnNode)
      returnNode
    }

    override def postVisit(expr: BooleanLiteral): Node = {
      constr.newConst(expr match {
        case TrueLiteral => 1
        case FalseLiteral => 0
      }, Mode.getb)
    }

    override def postVisit(expr: IntLiteral): Node = {
      constr.newConst(expr.value.toInt, Mode.getIs) // TODO: what about 2^31?
    }

    override def postVisit(expr: Apply, argumentResults: List[Node]): Node = {
      if (expr.decl.isEmpty) throw new IllegalArgumentException("Unresolved reference")
      expr.decl.get match {
        case Builtins.IntAddDecl =>
          constr.newAdd(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntSubDecl =>
          constr.newSub(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntMulDecl =>
          constr.newMul(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntDivDecl =>
          val divNode = constr.newDiv(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(divNode, Mode.getM, firm.nodes.Div.pnM))
          constr.newProj(divNode, Mode.getIs, firm.nodes.Div.pnRes)
        case Builtins.IntModDecl =>
          val modNode = constr.newMod(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(modNode, Mode.getM, firm.nodes.Mod.pnM))
          constr.newProj(modNode, Mode.getIs, firm.nodes.Mod.pnRes)

        case Builtins.IntLessDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Less)
        case Builtins.IntLessEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.LessEqual)
        case Builtins.IntGreaterDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Greater)
        case Builtins.IntGreaterEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.GreaterEqual)

        case Builtins.EqualsDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal)
        case Builtins.UnequalDecl =>
          val equalNode = constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal)
          constr.newNot(equalNode, Mode.getb)
        case Builtins.ArrayAccessDecl =>
          constr.newSel(argumentResults(0), argumentResults(1), firmType(Typer.getType(expr)))
        case decl =>
          // This *should* just work, but I can't test it because we have no way of getting an object reference
          // (as is needed for method calls as `this`-pointer
          call(methodEntity(decl), argumentResults.toArray)
      }
    }

    private val callocType = {
      val Iu = firmType(Builtins.ExtendedIntType)
      val params: Array[Type] = List(Iu, Iu).toArray
      // we can't construct a void*, so we take the next best thing: byte*
      val result: Array[Type] = Array(new PointerType(new PrimitiveType(Mode.getb)))
      new MethodType(params, result)
    }
    private val calloc = new Entity(P.getGlobalType, "calloc", callocType)

    private def call(methodEntity: Entity, args: Array[Node]): Node = {
      val addr = constr.newAddress(methodEntity)
      val call = constr.newCall(constr.getCurrentMem, addr, args, methodEntity.getType)
      constr.setCurrentMem(constr.newProj(call, Mode.getM, firm.nodes.Call.pnM))

      constr.newProj(
        constr.newProj(call, Mode.getT, firm.nodes.Call.pnTResult),
        callocType.getMode,
        0
      )
    }

    override def postVisit(expr: NewArray, _1: Unit, firstDimSize: Node): Node = {
      val baseType = firmType(expr.typ)
      val size = constr.newConst(baseType.getSizeBytes, Mode.getIu)
      val elems = constr.newConv(firstDimSize, Mode.getIu)
      call(calloc, Array[Node](elems, size))
    }

    override def postVisit(expr: NewObject, _1: Unit): Node = {
      val classType = firmClassEntity(expr.typ.decl.get).getType
      val size = constr.newConst(classType.getSizeBytes, Mode.getIu)
      val num_elems = constr.newConst(1, Mode.getIu)
      call(calloc, Array[Node](num_elems, size))
    }
  }

  override def findings: List[Finding] = List()
}
