package mjis.util

import mjis.ast._
import java.io.BufferedWriter
import scala.collection.immutable.StringOps

class PrettyPrinter(writer: BufferedWriter) {
  private var indent_level = 0
  private def emit(s: String) = writer.write(s)
  private def indent() = { indent_level += 1 }
  private def unindent() = { indent_level -= 1 }
  private def newLine(): Unit = {
    writer.newLine()
    for (_ <- 0 until indent_level) emit("\t")
  }

  def print(ast: Program): Unit = {
    ast.classes.sortBy{ _.name }.foreach { printClassDecl }
    writer.flush()
  }

  /// concerning whitespace:
  /// all print*-methods assume they're called "inline", and that the caller has
  /// handled all surrounding whitespace, with the exception of printConditionalBody,
  /// which only exists to reduce duplicate code between handling of If and While statements.

  private def printClassDecl(cl: ClassDecl): Unit = {
    emit(s"class ${cl.name} {")
    if (cl.methods.isEmpty && cl.fields.isEmpty)
      emit(" }")
    else {
      indent()
      for (m <- cl.methods.sortBy { _.name }) {
        newLine()
        printMethodDecl(m)
      }
      for (f <- cl.fields.sortBy { _.name }) {
        newLine()
        printFieldDecl(f)
      }
      unindent()
      newLine()
      emit("}")
    }
    newLine()
  }

  private def printConditionalBody(body: Statement): Unit = {
    if (body.isInstanceOf[Block]) {
      emit(" ")
      printBlock(body.asInstanceOf[Block])
    } else {
      indent()
      newLine()
      printStatement(body)
      unindent()
    }
  }

  private def printStatement(stmt: Statement): Unit = {
    stmt match {
      case If(condition, ifTrue, ifFalse) =>
        emit("if (")
        printExpression(condition, parens=false)
        emit(")")
        printConditionalBody(ifTrue)
        if (ifFalse != EmptyStatement) {
          if (ifTrue.isInstanceOf[Block]) emit(" ") else newLine()
          emit("else")
          if (ifFalse.isInstanceOf[Block] || ifFalse.isInstanceOf[If] || ifFalse.isInstanceOf[While])
            emit(" ")
          else
            newLine()
          printStatement(ifFalse)
        }
      case While(condition, body) =>
        emit("while (")
        printExpression(condition, parens=false)
        emit(")")
        printConditionalBody(body)
      case ReturnStatement(expr) =>
        emit("return")
        expr match {
          case None => ()
          case Some(expr) =>
            emit(" ")
            printExpression(expr, parens=false)
        }
        emit(";")
      case ExpressionStatement(expr) =>
        printExpression(expr, parens=false)
        emit(";")
      case LocalVarDeclStatement(name, typ, init) =>
        printType(typ)
        emit(" ")
        emit(name)
        init match {
          case Some(e) =>
            emit(" = ")
            printExpression(e, parens=false)
          case None => ()
        }
        emit(";")
      case block: Block => printBlock(block)
      case _ => ???
    }
  }

  private def printExpression(expr: Expression, parens: Boolean = true): Unit = {
    if (parens && !expr.isInstanceOf[Literal]) emit("(")
    expr match {
      case Assignment(lhs, rhs) =>
        printExpression(lhs)
        emit(" = ")
        printExpression(rhs)
      case application: Apply => printApply(application)
      case NewObject(typ) =>
        emit("new ")
        printType(typ)
        emit("()")
      case NewArray(base, firstDim, rest) =>
        emit("new ")
        printType(base)
        emit("[")
        printExpression(firstDim, parens=false)
        emit("]")
        for (i <- 0 until rest) emit("[]")
      case Select(qualifier, name) =>
        printExpression(qualifier)
        emit(".")
        emit(name)
      case Ident(name) => emit(name)
      case ThisLiteral => emit("this")
      case NullLiteral => emit("null")
      case IntLiteral(value) => emit(value)
      case TrueLiteral => emit("true")
      case FalseLiteral => emit("false")
    }
    if (parens && !expr.isInstanceOf[Literal]) emit(")")
  }

  // method invocations and operators need to be re-sugared
  private def printApply(invoc: Apply): Unit = {
    if (invoc.isOperator) {
      if (invoc.arguments.length == 2) {
        if (invoc.name == "[]") {
          // array access
          printExpression(invoc.arguments(0))
          emit("[")
          printExpression(invoc.arguments(1), parens=false)
          emit("]")
        } else {
          // other binary operators
          printExpression(invoc.arguments(0))
          emit(s" ${invoc.name} ")
          printExpression(invoc.arguments(1))
        }
      } else {
        // unary operators
        emit(invoc.name)
        printExpression(invoc.arguments(0))
      }
    } else {
      // normal method calls
      if (invoc.arguments(0) != ThisLiteral) {
        // all explicit `this` literals are stripped (there's no way to find out which ones were implicit anyways)
        printExpression(invoc.arguments(0))
        emit(".")
      }
      emit(invoc.name)
      emit("(")
      for (i <- 1 until invoc.arguments.length) {
        printExpression(invoc.arguments(i), parens=false)
        if (i < invoc.arguments.length - 1) emit(", ")
      }
      emit(")")
    }
  }

  private def printMethodDecl(method: MethodDecl): Unit = {
    emit("public ")
    if (method.isStatic)
      emit("static ")
    printType(method.typ)
    emit(" " + method.name + "(")
    var i = 0
    for (param <- method.parameters) {
      printType(param.typ)
      emit(" ")
      emit(param.name)
      i += 1
      if (method.parameters.length > i) emit(", ")
    }
    emit(") ")
    printBlock(method.body.asInstanceOf[Block])
  }

  private def printBlock(block: Block): Unit = {
    if (block.statements.forall{ _ == EmptyStatement })
      emit("{ }")
    else {
      emit("{")
      indent()
      for (stmt <- block.statements) {
        if (stmt != EmptyStatement) {
          newLine()
          printStatement(stmt)
        }
      }
      unindent()
      newLine()
      emit("}")
    }
  }

  private def printFieldDecl(field: FieldDecl): Unit = {
    emit("public ")
    printType(field.typ)
    emit(" ")
    emit(field.name)
    emit(";")
  }
  
  private def printType(typ: TypeDef): Unit = {
    typ match {
      case TypeBasic(name) => emit(name)
      case TypeArray(elementType) =>
        printType(elementType)
        emit("[]")      
    }
  }
}
