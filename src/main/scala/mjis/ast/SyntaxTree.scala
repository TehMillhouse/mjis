package mjis.ast

import scala.collection.mutable.ListBuffer

// NOTE: Representing names and types with String is not a good idea,
//       will be fixed as soon as appropriate types are implemented.

sealed trait SyntaxTree {
  def typ: TypeDef = ???
}

final case class Program(classes: List[ClassDecl]) extends SyntaxTree

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
  fields: List[FieldDecl]) extends Decl

sealed trait MemberDecl extends Decl

/* `typ` `name` (= `body`) */
final case class FieldDecl(
  name: String,
  override val typ: TypeDef) extends MemberDecl

/* `typ` `name`(`parameters`) { `body` } */
final case class MethodDecl(
  name: String,
  parameters: List[Parameter],
  override val typ: TypeDef,
  body: SyntaxTree,
  isStatic: Boolean = false) extends MemberDecl

/* `typ` `name` */
final case class Parameter(name: String, override val typ: TypeDef) extends SyntaxTree

sealed trait TypeDef extends SyntaxTree
final case class TypeBasic(name: String) extends TypeDef
final case class TypeArray(elementType: TypeDef) extends TypeDef

sealed trait Statement extends SyntaxTree

/* `typ` `name` ( = `body`) */
final case class LocalVarDeclStatement(name: String, override val typ: TypeDef, initializer: Option[Expression]) extends Statement
final case class Block(statements: List[Statement]) extends Statement
final case object EmptyStatement extends Statement
/* if (`condition`) { `ifTrue` } else { `ifFalse` } */
final case class If(condition: Expression, ifTrue: Statement, ifFalse: Statement) extends Statement
/* while (`condition`) { `block` } */
final case class While(condition: Expression, body: Statement) extends Statement
final case class ExpressionStatement(expr: Expression) extends Statement
final case class ReturnStatement(returnValue: Option[Expression]) extends Statement

sealed trait Expression extends SyntaxTree

/* `lhs` = `rhs` */
final case class Assignment(lhs: Expression, rhs: Expression) extends Expression
/* `name`(`args`) */
final case class Apply(name: String, arguments: List[Expression], isOperator: Boolean = false) extends Expression
/* new `typ`() */
final case class NewObject(override val typ: TypeBasic) extends Expression
/* new `typ`[firstDimSize][]^additionalDims */
final case class NewArray(override val typ: TypeBasic, firstDimSize: Expression, additionalDims: Int) extends Expression
/* `qualifier`.`name` */
final case class Select(qualifier: Expression, name: String) extends Expression

sealed trait Literal extends Expression

final case class Ident(name: String) extends Literal {
  override def toString = name
}
final case object ThisLiteral extends Literal
case object NullLiteral extends Literal
final case class IntLiteral(value: String) extends Literal
sealed abstract class BooleanLiteral(value: Boolean) extends Literal
case object TrueLiteral extends BooleanLiteral(true)
case object FalseLiteral extends BooleanLiteral(false)

