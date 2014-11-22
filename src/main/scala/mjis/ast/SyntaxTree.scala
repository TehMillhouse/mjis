package mjis.ast

import mjis._

/** Base trait for all syntax tree elements. */
sealed trait SyntaxTree {
  def accept[A](visitor: SyntaxTreeVisitor[A]): A
}

abstract class SyntaxTreeError(/* element: SyntaxTree */) extends Finding {
  def severity = Severity.ERROR
  override def pos: Position = new Position(0, 0, "") // TODO: element.position
}

/** A syntax tree element that has a reference to a Decl. */
sealed trait Ref[D <: Decl] extends SyntaxTree {
  private var _decl: Option[D] = None
  def decl = _decl
  def decl_=(value: D) = _decl match {
    case None => _decl = Some(value)
    case Some(decl) => if (decl != value) throw new IllegalStateException("Tried to set two different declarations for one Ref.")
  }
  val name: String
}

final case class Program(classes: List[ClassDecl]) extends SyntaxTree {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait Decl extends SyntaxTree {
  def name: String
}

/* class `name` {
   *   `methods`
   *   `fields`
   * }
   */
final case class ClassDecl(
  name: String,
  methods: List[MethodDecl],
  fields: List[FieldDecl]) extends Decl {

  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait TypedDecl extends Decl {
  val typ: TypeDef
}

/* `typ` `name` (= `body`) */
final case class FieldDecl(
  name: String,
  override val typ: TypeDef) extends TypedDecl {

  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

/* `typ` `name`(`parameters`) { `body` } */
final case class MethodDecl(
  name: String,
  parameters: List[Parameter],
  override val typ: TypeDef,
  body: Block,
  isStatic: Boolean = false) extends TypedDecl {

  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

/* `typ` `name` */
final case class Parameter(name: String, override val typ: TypeDef) extends TypedDecl {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait TypeDef extends SyntaxTree
final case class TypeBasic(name: String) extends TypeDef with Ref[ClassDecl] {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
final case class TypeArray(elementType: TypeBasic, numDimensions: Int = 1) extends TypeDef {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait Statement extends SyntaxTree

/* `typ` `name` ( = `body`) */
final case class LocalVarDeclStatement(name: String, override val typ: TypeDef, initializer: Option[Expression]) extends Statement with TypedDecl {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
final case class Block(statements: List[Statement]) extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
case object EmptyStatement extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* if (`condition`) { `ifTrue` } else { `ifFalse` } */
final case class If(condition: Expression, ifTrue: Statement, ifFalse: Statement) extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* while (`condition`) { `block` } */
final case class While(condition: Expression, body: Statement) extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
final case class ExpressionStatement(expr: Expression) extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
final case class ReturnStatement(returnValue: Option[Expression]) extends Statement {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait Expression extends SyntaxTree

/* `lhs` = `rhs` */
final case class Assignment(lhs: Expression, rhs: Expression) extends Expression {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* `name`(`args`) */
final case class Apply(name: String, arguments: List[Expression], isOperator: Boolean = false) extends Expression with Ref[MethodDecl] {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* new `typ`() */
final case class NewObject(typ: TypeBasic) extends Expression {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* new `typ`[firstDimSize][]^additionalDims */
final case class NewArray(typ: TypeBasic, firstDimSize: Expression, additionalDims: Int) extends Expression {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
/* `qualifier`.`name` */
final case class Select(qualifier: Expression, name: String) extends Expression with Ref[FieldDecl] {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}

sealed trait Literal extends Expression

final case class Ident(name: String) extends Literal with Ref[Decl] {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
  override def toString = name
}
final case class ThisLiteral() extends Literal with Ref[TypedDecl] {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
  override val name = "this"
}
case object NullLiteral extends Literal {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
final case class IntLiteral(value: String) extends Literal {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
sealed abstract class BooleanLiteral(value: Boolean) extends Literal {
  override def accept[A](visitor: SyntaxTreeVisitor[A]): A = visitor.visit(this)
}
case object TrueLiteral extends BooleanLiteral(true)
case object FalseLiteral extends BooleanLiteral(false)
