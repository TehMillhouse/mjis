package mjis

import scala.collection.mutable.{ListBuffer, Queue, Map => MutableMap}
import java.io.BufferedWriter
import java.nio.file.{Path, Paths}
import mjis.TokenData._
import mjis.util.LineReader
import mjis.util.Trie

object Lexer {
  case class UnclosedCommentError(pos: Position) extends Finding {
    def msg = "unclosed comment"
    def severity = Severity.ERROR
  }
  case class UnknownTokenError(pos: Position) extends Finding {
    def msg = s"unknown token"
    def severity = Severity.ERROR
  }

  // Don't pay the Unicode performance penalty ...
  implicit class ASCIIChar(val ch: Char) extends AnyVal {
    def isASCIIDigit = ch >= '0' && ch <= '9'
    def isASCIILetter = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
    def isASCIILetterOrDigit = ch.isASCIIDigit || ch.isASCIILetter
  }
}

class Lexer(val inputReader: java.io.Reader) extends AnalysisPhase[LookaheadIterator[Token]] {
  import Lexer.ASCIIChar

  // abstraction over constant-length (quasi-)tokens
  private abstract class Symbol()
  private case class TokenSymbol(data: TokenData) extends Symbol
  private case object LineBreak extends Symbol
  private case object CommentStart extends Symbol

  private val whitespace = Set[Char](' ', '\t', '\r', '\n')
  private val constLenTokens = List[TokenData](Unequal, Not, ParenOpen, ParenClosed,
    Mult, Plus, Comma, Minus, Dot, Divide, Semicolon, SmallerEquals, Smaller, Equals,
    Assign, GreaterEquals, Greater, Modulo, LogicalAnd, SquareBracketOpen,
    SquareBracketClosed, CurlyBraceOpen, CurlyBraceClosed, LogicalOr)
  private val unusedConstLenTokens = List[String]("*=", "++", "+=", "-=", "--", "/=",
    ":", "<<=", "<<", ">>=", ">>>=", ">>>", ">>", "?", "%=", "&=", "&", "^=", "^", "~", "|", "|=")
  private val symbols = new Trie[Symbol](
    constLenTokens.map(t => (t.literal, TokenSymbol(t))) ++
    unusedConstLenTokens.map(t => (t, TokenSymbol(UnusedFeature(t)))) :+
    ("/*" -> CommentStart)
  )
  private val keywords: Map[String, TokenData] = List[TokenData](BooleanType, Class, Else, False, If, IntType, New, Null,
    Public, Return, Static, This,True, VoidType, While).map(t => (t.literal, t)).toMap
  private val unusedKeywords = Set[String]("abstract", "assert", "break", "byte", "case", "catch",
    "char", "const", "continue", "default", "double", "do", "enum", "extends", "finally",
    "final", "float", "for", "goto", "implements", "import", "instanceof", "interface",
    "long", "native", "package", "private", "protected", "short", "strictfp", "super", "switch",
    "synchronized", "throws", "throw", "transient", "try", "volatile")

  private val identifierCache = MutableMap[String, Identifier]()
  private val input = new LineReader(inputReader)
  private val _findings = ListBuffer.empty[Finding]

  def this(input: String) = {
    this(new java.io.StringReader(input))
  }

  /** Somewhat efficient implementation.
    * Assumes p does not accept \n.
    */
  def takeWhile(p: Char => Boolean): String = {
    val end = (input.offset until input.source.length())
      .find(i => !p(input.source(i)))
      .getOrElse(input.source.length())
    val result = input.source.substring(input.offset, end)
    input.offset = end
    result
  }

  private def lexInteger(): Token = {
    val pos = input.pos
    new Token(IntegerLiteral(takeWhile(_.isASCIIDigit)), pos)
  }

  private def lexIdentifier(): Token = {
    val pos = input.pos
    val ident = takeWhile(c => c.isASCIILetterOrDigit || c == '_')
    val data =
      if (keywords.contains(ident)) keywords(ident)
      else if (unusedKeywords(ident)) UnusedFeature(ident)
      else identifierCache.getOrElseUpdate(ident, Identifier(ident))
    new Token(data, pos)
  }

  /** parse remainder after "/ *"  while memorizing its original position */
  @annotation.tailrec
  private def lexCommentRemainder(startPos: Position): Unit = {
    if (input.atEnd) {
      _findings += Lexer.UnclosedCommentError(startPos)
    } else if (!(input.consume() == '*' && input.consume() == '/')) {
      lexCommentRemainder(startPos)
    }
  }

  @annotation.tailrec
  private def lexToken(): Token = {
    if (input.atEnd) new Token(TokenData.EOF, input.pos)
    else if (input.currentChar == '_' || input.currentChar.isASCIILetter)
      lexIdentifier()
    else if (input.currentChar == '0') {
      // special case: always a single token
      val token = new Token(TokenData.IntegerLiteral("0"), input.pos)
      input.consume()
      token
    } else if (input.currentChar.isASCIIDigit)
      lexInteger()
    else if (whitespace(input.currentChar)) {
      input.consume()
      lexToken()
    } else {
      val pos = input.pos
      symbols.tryLookupLongestPrefix(input) match {
        case None =>
          _findings += Lexer.UnknownTokenError(pos)
          while (!whitespace(input.currentChar) && !input.atEnd) input.consume()
          lexToken()
        case Some(symbol) =>
          symbol match {
            case TokenSymbol(data) => new Token(data, pos)
            case LineBreak => lexToken()
            case CommentStart =>
              lexCommentRemainder(pos)
              lexToken()
          }
      }
    }
  }

  class TokenIterator() extends LookaheadIterator[Token] {
    /// behaves as follows: `next()` is guaranteed to always return a valid
    /// token, and will generate arbitrary many EOF tokens, but `hasNext()`
    /// will return false after the first EOF has been returned by `next()`
    private val _buffer: Queue[Token] = Queue[Token](lexToken())
    private var _hasNext = true
    private val _buffer_size = 3
    override def next(): Token = {
      val oldHead = _buffer.dequeue
      if (oldHead.data != EOF)
        while (_buffer.length < _buffer_size) _buffer.enqueue(lexToken())
      else
        _hasNext = false
      oldHead
    }

    override def hasNext: Boolean = _hasNext
    def peek(n: Int = 1): Token = {
      if (n < _buffer_size)
        _buffer.get(n).getOrElse(peek(n-1))
      else
        throw new ArrayIndexOutOfBoundsException("peek() only supports lookahead < " + _buffer_size)
    }
    override def head: Token = _buffer.head
  }


  protected override def getResult(): LookaheadIterator[Token] = new TokenIterator()
  override def forceResult(): Unit = result.length

  override def findings: List[Finding] = _findings.toList

  override def dumpResult(writer: BufferedWriter): Unit = {

    for (token <- result) {
      token.data match {
        case Identifier(literal) => writer.write(s"identifier $literal")
        case IntegerLiteral(literal) => writer.write(s"integer literal $literal")
        case _ => writer.write(token.data.literal)
      }
      writer.newLine()
    }

    writer.flush()
  }
}

trait LookaheadIterator[T] extends BufferedIterator[T] {
  def peek(n: Int): T
}
