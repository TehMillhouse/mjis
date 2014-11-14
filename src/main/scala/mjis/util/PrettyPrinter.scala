package mjis.util

import mjis.ast._
import java.io.BufferedWriter
import scala.collection.immutable.StringOps

class PrettyPrinter(writer: BufferedWriter) {
  private var indent_level = 0
  private val tab = "\t"
  private def Indent() = { indent_level += 1 }
  private def Unindent() = { indent_level -= 1 }
  private def WriteIndentation(): Unit = { for (_ <- 0 until indent_level) emit(tab) }
  private def emit(s: String) = writer.write(s)

  def print(ast: Program): Unit = {
    for (cl <- ast.classes) printClassDecl(cl)
  }

  private def printClassDecl(cl: ClassDecl): Unit = {
    emit("class " + cl.name + " {")
    if (cl.methods.isEmpty && cl.fields.isEmpty)
      emit(" }")
    else {
      writer.newLine()
      Indent()
      for (m <- cl.methods.sortBy { _.name }) { printMethodDecl(m) }
      for (f <- cl.fields.sortBy { _.name }) { printFieldDecl(f) }
      Unindent()
      emit("}")
    }
    writer.newLine()
  }

  // there are special pretty-printing rules for when things are on the same line, hence this
  private def printConditionalBody(stmt: Statement, isElse: Boolean = false, trailingNewline: Boolean = true): Unit = {
    stmt match {
      case If(_, _, _) | While(_, _) =>
        if (isElse) {
          emit(" ")
          printStatement(stmt)
        } else {
          writer.newLine()
          Indent()
          printStatement(stmt)
          Unindent()
        }
      case Block(_) =>
        emit(" ")
        printBlock(stmt.asInstanceOf[Block], trailingNewline)
        if (!trailingNewline) emit(" ")
      case _ =>
        writer.newLine()
        Indent()
        printStatement(stmt)
        Unindent()
    }
  }
  
  private def printStatement(stmt: Statement): Unit = {
    stmt match {
      case If(condition, ifTrue, ifFalse) =>
        emit("if (")
        printExpression(condition, false)
        emit(")")
        val hasElse = ifFalse != EmptyStatement
        printConditionalBody(ifTrue, trailingNewline = !hasElse)
        if (hasElse) {
          emit("else")
          printConditionalBody(ifFalse, true)
        }
      case While(condition, ifTrue) =>
        emit("while (")
        printExpression(condition, false)
        emit(")")
        printConditionalBody(ifTrue)
        writer.newLine()
      case ReturnStatement(expr) =>
        emit("return")
        expr match {
          case None => ()
          case Some(expr) =>
            emit(" ")
            printExpression(expr, false)
        }
        emit(";")
        writer.newLine()
      case ExpressionStatement(expr) =>
        printExpression(expr, false)
        emit(";")
        writer.newLine()
      case LocalVarDeclStatement(name, typ, init) =>
        printType(typ)
        emit(" ")
        emit(name)
        init match {
          case Some(e) =>
            emit(" = ")
            printExpression(e, false)
          case None => ()
        }
        emit(";")
        writer.newLine()
      case Block(_) => printBlock(stmt.asInstanceOf[Block])
      case EmptyStatement => ()
    }
  }

  private def printExpression(expr: Expression, parens: Boolean = true): Unit = {
    if (parens && !expr.isInstanceOf[Literal]) emit("(")
    expr match {
      case Assignment(lhs, rhs) =>
        printExpression(lhs)
        emit(" = ")
        printExpression(rhs)
      case Apply(_, _) => printApply(expr.asInstanceOf[Apply])
      case NewObject(typ) =>
        emit("new ")
        printType(typ)
        emit("()")
      case NewArray(base, firstDim, rest) =>
        emit("new ")
        printType(base)
        emit("[")
        printExpression(firstDim, false)
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
    invoc.name match {
      case "==" | "!=" | "+" | "-" | "&&" | "||" | "<" | ">" | "<=" | ">=" | "*" | "/" | "%" =>
        printExpression(invoc.arguments(0))
        emit(" ")
        emit(invoc.name)
        emit(" ")
        printExpression(invoc.arguments(1))
      case "!" | "- (unary)" =>
        if (invoc.name == "!")
          emit(invoc.name)
        else
          emit("-")  // Ugh, let's find a better way to do this.
        printExpression(invoc.arguments(0))
      case "[]" =>
        printExpression(invoc.arguments(0))
        emit("[")
        printExpression(invoc.arguments(1), false)
        emit("]")
      case _ =>
        if (invoc.arguments(0) != ThisLiteral) {
          // all explicit `this` literals are stripped (there's no way to find out which ones were implicit anyways)
          printExpression(invoc.arguments(0))
          emit(".")
        }
        emit(invoc.name)
        emit("(")
        for (i <- 1 until invoc.arguments.length) {
          printExpression(invoc.arguments(i), false)
          if (i < invoc.arguments.length - 1) emit(", ")
        }
        emit(")")
    }
  }

  // TODO: properly handle main method
  private def printMethodDecl(method: MethodDecl): Unit = {
        WriteIndentation()
        emit("public ")
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
  
  private def printBlock(block: Block, trailingNewline: Boolean = true): Unit = {
    if (block.statements.isEmpty)
      emit("{ }")
    else {
      emit("{")
      Indent()
      writer.newLine()
      for (stmt <- block.statements) {
        WriteIndentation()
        printStatement(stmt)
      }
      Unindent()
      WriteIndentation()
      emit("}")
    }
    if (trailingNewline) writer.newLine()

  }

  private def printFieldDecl(field: FieldDecl): Unit = {
    WriteIndentation()
    emit("public ")
    printType(field.typ)
    emit(" ")
    emit(field.name)
    emit(";")
    writer.newLine()
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