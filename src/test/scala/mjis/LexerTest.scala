package mjis

import mjis.TokenData._
import org.scalatest._
import java.io.{StringWriter, BufferedWriter}

class LexerTest extends FlatSpec with Matchers with Inspectors {

  def checkMatchesTokenData(tokenIt: Iterator[Token], expected: Seq[TokenData]): Unit = {
    val tokens = tokenIt.toSeq
    tokens should have length expected.length + 1
    for ((token, expectedTokenData) <- tokens zip expected) {
      token.data should === (expectedTokenData)
    }
    tokens.last.data should === (EOF)
  }

  /* Tests start here */

  "The lexer" should "return only the EOF token when parsing the empty string" in {
    val lexer = new Lexer("")
    checkMatchesTokenData(lexer.result, Seq())
    lexer.findings shouldBe empty
    lexer.success shouldBe true
  }

  it should "implement BufferedIterator correctly" in {
    val lexer = new Lexer("a")
    lexer.result.head.data shouldBe Identifier("a")
    lexer.result.hasNext shouldBe true
    lexer.result.next().data shouldBe Identifier("a")
    lexer.result.next()  // skip EOF
    lexer.result.hasNext shouldBe false
  }

  it should "separate tokens by whitespace or comments" in {
    val lexer = new Lexer("a a  a\ta\ra\na\r\na/*comment*/a")
    checkMatchesTokenData(lexer.result, Range(0, 8) map { _ => new Identifier("a") })
    lexer.success shouldBe true
  }

  it should "recover after an invalid token" in {
    val lexer = new Lexer("a # b")
    checkMatchesTokenData(lexer.result, Seq(new Identifier("a"), new Identifier("b")))
  }

  it should "skip until whitespace after an invalid token" in {
    val lexer = new Lexer("a #class b")
    checkMatchesTokenData(lexer.result, Seq(new Identifier("a"), new Identifier("b")))
  }

  it should "reach EOF when the input ends with an invalid token" in {
    val lexer = new Lexer("a #")
    val res = lexer.result.toSeq
    res.length shouldBe 2
    res.head.data should === (new Identifier("a"))
    res.last.data shouldBe EOF
  }

  it should "recognize other whitespace" in {
    val lexer = new Lexer("\r\n\t")
    checkMatchesTokenData(lexer.result, Seq())
    lexer.success shouldBe true
  }

  it should "not recognize unspecified whitespace" in {
    val lexer = new Lexer("\f")
    checkMatchesTokenData(lexer.result, Seq())
    lexer.success shouldBe false
    val finding = lexer.findings.head
    finding shouldBe a [Lexer.UnknownTokenError]
    finding.pos.column shouldBe 1
    finding.pos.line shouldBe 1
  }

  it should "not recognize the EOF char" in {
    val lexer = new Lexer("\u001a")
    checkMatchesTokenData(lexer.result, Seq())
    lexer.success shouldBe false
  }

  it should "separate operators and keywords by comments" in {
    val lexer = new Lexer("&/*comment*/& whi/*comment*/le")
    checkMatchesTokenData(lexer.result, List(new UnusedFeature("&"), new UnusedFeature("&"),
      new Identifier("whi"), new Identifier("le")))
    lexer.success shouldBe true
  }

  it should "separate tokens by operator symbols, even without whitespace" in {
    val lexer = new Lexer("{a(!a.x&&b!=c[d])+5,b;}")
    val expected = List(
      CurlyBraceOpen, new Identifier("a"), ParenOpen, Not, new Identifier("a"), Dot,
      new Identifier("x"), LogicalAnd, new Identifier("b"), Unequal, new Identifier("c"),
      SquareBracketOpen, new Identifier("d"), SquareBracketClosed, ParenClosed,
      Plus, new IntegerLiteral("5"), Comma, new Identifier("b"), Semicolon,
      CurlyBraceClosed
    )
    checkMatchesTokenData(lexer.result, expected)
    lexer.success shouldBe true
  }

  it should "not choke on many consecutive comments" in {
    val lexer = new Lexer((Range(0, 10000) map { _ => "/**/" }).mkString(""))
    checkMatchesTokenData(lexer.result, Seq())
    lexer.success shouldBe true
  }
  it should "not choke on many consecutive line breaks" in {
    val lexer = new Lexer((Range(0, 10000) map { _ => "\n" }).mkString(""))
    checkMatchesTokenData(lexer.result, Seq())
    lexer.success shouldBe true
  }

  it should "set line/char of tokens correctly" in {
    val input = "a aa      a\t a\na a\na a\r\na a\n\na a "
    val expected = List( // (line, char)
      (1, 1),
      (1, 3),
      (1, 11),
      (1, 14), // Tab is counted as one character
      (2, 1),
      (2, 3),
      (3, 1), // Single \n is counted as newline
      (3, 3),
      (4, 1), // \r\n is counted as one newline
      (4, 3),
      (6, 1), // \n\n is two newlines
      (6, 3),
      (6, 5)  // EOF should be one char after the last char in the string
    )

    val lexer = new Lexer(input)
    val result = lexer.result.toSeq
    lexer.success shouldBe true
    result should have length expected.length
    for ((token, lineAndChar) <- result zip expected) {
      (token.pos.line, token.pos.column) should equal(lineAndChar)
    }
  }

  it should "recognize all MiniJava operator symbols" in {
    val lexer = new Lexer("!= ! ( ) * + , - . / ; <= < == = >= > % && [ ] { } ||")
    checkMatchesTokenData(lexer.result, List(Unequal, Not, ParenOpen, ParenClosed,
      Mult, Plus, Comma, Minus, Dot, Divide, Semicolon, SmallerEquals, Smaller, Equals,
      Assign, GreaterEquals, Greater, Modulo, LogicalAnd, SquareBracketOpen,
      SquareBracketClosed, CurlyBraceOpen, CurlyBraceClosed, LogicalOr))
    lexer.success shouldBe true
  }

  it should "recognize all other Java operator symbols" in {
    val expected = List("*=", "++", "+=", "-=", "--", "/=", ":", "<<=", "<<", ">>=", ">>>=", ">>>",
      ">>", "?", "%=", "&=", "&", "^=", "^", "~", "|", "|=")
    val lexer = new Lexer(expected.mkString(" "))
    checkMatchesTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
    lexer.success shouldBe true
  }

  it should "recognize all MiniJava keywords" in {
    val lexer = new Lexer("boolean class else false if int new null public return "
      + "static this true void while")
    checkMatchesTokenData(lexer.result, List(BooleanType, Class, Else, False, If, IntType,
      New, Null, Public, Return, Static, This, True, VoidType, While))
    lexer.success shouldBe true
  }

  it should "recognize all other Java keywords" in {
    val expected = List("abstract", "assert", "break", "byte", "case", "catch",
      "char", "const", "continue", "default", "double", "do", "enum", "extends", "finally",
      "final", "float", "for", "goto", "implements", "import", "instanceof", "interface",
      "long", "native", "package", "private", "protected", "short", "strictfp", "super", "switch",
      "synchronized", "throws", "throw", "transient", "try", "volatile")
    val lexer = new Lexer(expected.mkString(" "))
    checkMatchesTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
    lexer.success shouldBe true
  }

  it should "recognize Integer literals" in {
    val expected = List(0, 1234567890, 12, 23, 34, 45, 56, 67, 78, 89, 90)
    val lexer = new Lexer(expected.mkString(" "))
    checkMatchesTokenData(lexer.result, expected map { i => IntegerLiteral(i.toString) })
    lexer.success shouldBe true
  }

  it should "recognize identifiers" in {
    val expected = List("ident", "ident42", "ident0815", "ident_", "_ident42", "_", "__", "_42",
      "superman", "if0", "_if", "if_true", "iftrue")
    val lexer = new Lexer(expected.mkString(" "))
    checkMatchesTokenData(lexer.result, expected map { s => Identifier(s) })
    lexer.success shouldBe true
  }

  it should "parse '/ *' and '* /' as tokens, not comment start/end" in {
    val lexer = new Lexer("a / * abc * / c")
    checkMatchesTokenData(lexer.result, List(new Identifier("a"), Divide, Mult,
      new Identifier("abc"), Mult, Divide, new Identifier("c")))
    lexer.success shouldBe true
  }

  it should "ignore '*/' outside comments" in {
    val lexer = new Lexer("/* this is a /* comment */ a */ b")
    checkMatchesTokenData(lexer.result, List(new Identifier("a"), Mult, Divide, new Identifier("b")))
    lexer.success shouldBe true
  }

  it should "ignore multiple '*' in comments" in {
    val lexer = new Lexer("/*** abc ***/")
    checkMatchesTokenData(lexer.result, List[TokenData]())
    lexer.success shouldBe true
  }

  it should "ignore tokens and '/*' in comments" in {
    val lexer = new Lexer("/* while(true) { int i = 42; i++; } /* */")
    checkMatchesTokenData(lexer.result, List[TokenData]())
    lexer.success shouldBe true
  }

  it should "parse EOF as identifier" in {
    val lexer = new Lexer("EOF")
    checkMatchesTokenData(lexer.result, List(new Identifier("EOF")))
    lexer.success shouldBe true
  }

  it should "parse greedily" in {
    val testCases = List(
      ("<<<<<",  List(new UnusedFeature("<<"), new UnusedFeature("<<"), Smaller)),
      ("<<<=",   List(new UnusedFeature("<<"), SmallerEquals)),
      ("<<<<=", List(new UnusedFeature("<<"), new UnusedFeature("<<="))),
      ("<<<<<=", List(new UnusedFeature("<<"), new UnusedFeature("<<"), SmallerEquals)),
      (">>>>",   List(new UnusedFeature(">>>"), Greater)),
      (">>>>>",  List(new UnusedFeature(">>>"), new UnusedFeature(">>"))),
      (">>>>>>", List(new UnusedFeature(">>>"), new UnusedFeature(">>>"))),
      (">>>>=",  List(new UnusedFeature(">>>"), GreaterEquals)),
      ("^^^=",   List(new UnusedFeature("^"), new UnusedFeature("^"), new UnusedFeature("^="))),
      ("||=",    List(LogicalOr, Assign)),
      ("|||=",   List(LogicalOr, new UnusedFeature("|="))),
      ("||||=",  List(LogicalOr, LogicalOr, Assign)),
      ("+++=",   List(new UnusedFeature("++"), new UnusedFeature("+="))),
      ("++++=",  List(new UnusedFeature("++"), new UnusedFeature("++"), Assign)),
      ("---=",   List(new UnusedFeature("--"), new UnusedFeature("-="))),
      ("----=",  List(new UnusedFeature("--"), new UnusedFeature("--"), Assign)),
      ("=====",  List(Equals, Equals, Assign)),
      ("&&&&&",  List(LogicalAnd, LogicalAnd, new UnusedFeature("&"))),
      ("|||||",  List(LogicalOr, LogicalOr, new UnusedFeature("|"))),
      ("!!!=",   List(Not, Not, Unequal))
    )

    for (((input, expected), i) <- testCases.zipWithIndex) {
      val lexer = new Lexer(input)
      withClue(s"Test case $i") { checkMatchesTokenData(lexer.result, expected) }
      lexer.success shouldBe true
    }
  }

  it should "parse 0 at the start of an integer literal as separate token" in {
    val lexer = new Lexer("a 00815")
    checkMatchesTokenData(lexer.result, List(new Identifier("a"), new IntegerLiteral("0"),
      new IntegerLiteral("0"), new IntegerLiteral("815")))
    lexer.success shouldBe true
  }

  /* Failure cases */

  it should "fail on an unterminated comment" in {
    val lexer = new Lexer("a /* this is an unterminated comment")
    checkMatchesTokenData(lexer.result, List(new Identifier("a")))
    lexer.success shouldBe false
    val finding = lexer.findings.head
    finding shouldBe a [Lexer.UnclosedCommentError]
    finding.pos.line shouldBe 1
    finding.pos.column shouldBe 3
  }

  /* Result dumping */

  it should "dump the result in the format specified on assignment sheet 2" in {
    // Example from the assignment sheet
    val lexer = new Lexer(
      """/*
        | * @author Beate Best
        | */
        |class classic {
        |  public int method(int arg) {
        |    int res = arg+42;
        |    res >>= 4;
        |    return res;
        |  }
        |}
        |""".stripMargin)
    val stw = new StringWriter()
    val buf = new BufferedWriter(stw)
    lexer.dumpResult(buf)
    stw.toString() should equal (
      """class
        |identifier classic
        |{
        |public
        |int
        |identifier method
        |(
        |int
        |identifier arg
        |)
        |{
        |int
        |identifier res
        |=
        |identifier arg
        |+
        |integer literal 42
        |;
        |identifier res
        |>>=
        |integer literal 4
        |;
        |return
        |identifier res
        |;
        |}
        |}
        |EOF
""".stripMargin)
    buf.close()
  }

}
