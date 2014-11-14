package mjis

import mjis.ast._
import org.scalatest._
import CompilerTestMatchers._

class ParserTest extends FlatSpec with Matchers with Inspectors {

  def parseProgram(program: String) = {
    val lexer = new Lexer(program)
    val parser = new Parser(lexer.result)
    // The lexer result is only traversable once, so evaluate it only after the parser has finished.
    parser.result
    lexer should succeedLexing()
    parser
  }
  def parseStatements(statements: String) = parseProgram(
    "class Test { public void test() {" + System.lineSeparator() + statements + System.lineSeparator() + "} }"
  )
  def statementsAST(innerAST: Statement*) = Program(List(
    ClassDecl("Test", List(
      MethodDecl("test", List(), TypeBasic("void"),
        Block(innerAST.toList)
      )
    ), List())
  ))
  def expressionsAST(innerAST: Expression*) = statementsAST(innerAST.map(ExpressionStatement): _*)

  def repeat(str: String, count: Integer) = Seq.fill(count)(str).mkString("")

  /* class declarations */

  "The parser" should "accept an empty program" in {
    parseProgram("") should succeedParsingWith(Program(Nil))
  }

  it should "accept a program consisting of an empty class" in {
    parseProgram("class Test { }") should succeedParsingWith(Program(List(ClassDecl("Test", Nil, Nil))))
  }

  it should "accept many empty classes" in {
    parseProgram(repeat("class A { }", 10000)) should succeedParsing()
  }

  it should "accept fields of any type" in {
    parseProgram("class C { public int x; public boolean y; public void z; public MyType u; public MyType[][] v;}") should
      succeedParsingWith(
        Program(List(
          ClassDecl("C", Nil, List(
            FieldDecl("x", TypeBasic("int")),
            FieldDecl("y", TypeBasic("boolean")),
            FieldDecl("z", TypeBasic("void")),
            FieldDecl("u", TypeBasic("MyType")),
            FieldDecl("v", TypeArray(TypeArray(TypeBasic("MyType")))))))))
  }

  it should "accept many fields, main methods and methods" in {
    parseProgram("class C {"
        + repeat("""|public int x;
                    |public static void main(String[] args) {}
                    |public int z(int j, A b) {}""".stripMargin, 
                    10000) + "}") should succeedParsing()
  }

  it should "accept main methods with any name" in {
    parseProgram("class C { public static void foobar(String[] args) {} }") should succeedParsingWith(Program(List(
      ClassDecl("C", List(
        MethodDecl("foobar", List(Parameter("args", TypeArray(TypeBasic("String")))), TypeBasic("void"), Block(List()), isStatic=true)
      ), List())
    )))
  }

  /* statements */

  it should "accept an empty block" in {
    parseStatements("") should succeedParsingWith(statementsAST())
  }

  it should "accept a nested empty block" in {
    parseStatements("{}") should succeedParsingWith(statementsAST(Block(List())))
  }

  it should "accept a program with different local variable declarations" in {
    parseStatements("int a; boolean b; myType[] c = xyz; myType x = 42;") should succeedParsingWith(statementsAST(
      LocalVarDeclStatement("a", TypeBasic("int"), None),
      LocalVarDeclStatement("b", TypeBasic("boolean"), None),
      LocalVarDeclStatement("c", TypeArray(TypeBasic("myType")), Some(Ident("xyz"))),
      LocalVarDeclStatement("x", TypeBasic("myType"), Some(IntLiteral("42")))
    ))
  }

  it should "accept many nested blocks" in {
    // {{{{...}}{{...}}}}
    parseStatements(repeat("{", 10000) + repeat("}", 5000)
      + repeat("{", 5000) + repeat("}", 10000)) should succeedParsing()
  }

  it should "accept many nested if-else constructs" in {
    // if(1){if(1){...}}else{if(1){...}}
    parseStatements(repeat("if(1){", 10000) + repeat("}", 10000)
      + "else " + repeat("if(1){", 10000) + repeat("}", 10000)) should succeedParsing()
  }

  it should "accept many nested and consecutive while loops" in {
    parseStatements(repeat("while(0) {", 10000) + repeat("}", 10000)
        + repeat("while(0);", 10000)) should succeedParsing()
  }
  
  it should "accept a program with many return statements" in {
    parseStatements(repeat("return 0;", 10000)) should succeedParsing()
  }

  it should "properly recognize expression statements" in {
    // this is interesting because it's a spot where the grammar isn't SLL(1)
    parseProgram("class a { public void foo ( ) { a [ 2 ] ; } }") should succeedParsingWith(Program(List(
      ClassDecl("a", List(
        MethodDecl("foo", List(), TypeBasic("void"), Block(List(
          ExpressionStatement(Apply("[]", List(Ident("a"), IntLiteral("2")), isOperator=true))
        )))
      ), List())
    )))
  }

  /* expressions */

  it should "accept primary expressions" in {
    parseStatements(
      """
        |null;
        |false;
        |true;
        |1337;
        |myVar;
        |myFunc();
        |this;
        |(null);
        |new myType();
        |new myType[3+x][][];
        |new int[3+x][][];
      """.stripMargin) should succeedParsingWith(expressionsAST(
      NullLiteral,
      FalseLiteral,
      TrueLiteral,
      IntLiteral("1337"),
      Ident("myVar"),
      Apply("myFunc", List(ThisLiteral)),
      ThisLiteral,
      NullLiteral,
      NewObject(TypeBasic("myType")),
      NewArray(TypeBasic("myType"), Apply("+", List(IntLiteral("3"), Ident("x")), isOperator=true), 2),
      NewArray(TypeBasic("int"), Apply("+", List(IntLiteral("3"), Ident("x")), isOperator=true), 2)
    ))
  }

  it should "accept assignments" in {
    parseStatements("a=a;\na=a=a;") should succeedParsingWith(expressionsAST(
      Assignment(Ident("a"), Ident("a")),
      Assignment(Ident("a"), Assignment(Ident("a"), Ident("a")))
    ))
  }

  it should "accept long assignment chains" in {
    // a=a=a=...=a;
    parseStatements(repeat("a=", 10000) + "a;") should succeedParsing()
  }

  it should "accept method calls with parameters" in {
    parseStatements("a(b);\na(b, c);\na(b, c(d));") should succeedParsingWith(expressionsAST(
      Apply("a", List(ThisLiteral, Ident("b"))),
      Apply("a", List(ThisLiteral, Ident("b"), Ident("c"))),
      Apply("a", List(ThisLiteral, Ident("b"), Apply("c", List(ThisLiteral, Ident("d")))))
    ))
  }

  it should "accept long method call chains" in {
    // a(a(...a(null)...));
    parseStatements(repeat("a(", 10000) + "null" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept long parenthesis chains" in {
    // ((((...((a))...))));
    parseStatements(repeat("(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept long chains of alternating expressions and parentheses" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    parseStatements(repeat("a+(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept unary expressions" in {
    parseStatements("-c;\n-(-c);\n!c;\n!!c;\n!-!-c;\n!(-(!(-(c))));") should succeedParsingWith(expressionsAST(
      Apply("-", List(Ident("c")), isOperator=true),
      Apply("-", List(Apply("-", List(Ident("c")), true)), true),
      Apply("!", List(Ident("c")), true),
      Apply("!", List(Apply("!", List(Ident("c")), true)), true),
      Apply("!", List(Apply("-", List(Apply("!", List(Apply("-", List(Ident("c")), true)), true)), true)), true),
      Apply("!", List(Apply("-", List(Apply("!", List(Apply("-", List(Ident("c")), true)), true)), true)), true)
    ))
  }

  it should "accept long chains of unary expressions" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    parseStatements(repeat("!-", 10000) + "a;") should succeedParsing()
  }

  it should "accept binary expressions" in {
    parseStatements("a+b;a-b;a*b;a/b;a%b;a&&b;a||b;a>b;a<b;a<=b;a>=b;a==b;a!=b;") should succeedParsingWith(expressionsAST(
      List("+", "-", "*", "/", "%", "&&", "||", ">", "<", "<=", ">=", "==", "!=").map(Apply(_, List(Ident("a"), Ident("b")), isOperator=true)): _*
    ))
  }

  it should "accept binary expression chains" in {
    parseStatements("a+b*c+d;") should succeedParsingWith(expressionsAST(
      Apply("+", List(
        Apply("+", List(
          Ident("a"),
          Apply("*", List(
            Ident("b"),
            Ident("c")
          ), isOperator=true)
        ), isOperator=true),
        Ident("d")
      ), isOperator=true)
    ))
  }

  it should "accept nested binary expressions" in {
    parseStatements("a+b*c-d!=(e>f*g);") should succeedParsingWith(expressionsAST(
      Apply("!=", List(
        Apply("-", List(
          Apply("+", List(
            Ident("a"),
            Apply("*", List(
              Ident("b"),
              Ident("c")),
              isOperator=true
            )
          ), true),
          Ident("d")
        ), true),
        Apply(">", List(
          Ident("e"),
          Apply("*", List(
            Ident("f"),
            Ident("g")
          ), true)
        ), true)
      ), true)
    ))
  }

  it should "accept nested expressions containing assignments" in {
    parseStatements("a||b=c||d;") should succeedParsingWith(expressionsAST(
      Assignment(
        Apply("||", List(
          Ident("a"),
          Ident("b")
        ), isOperator=true),
        Apply("||", List(
          Ident("c"),
          Ident("d")
        ), true)
      )
    ))
  }

  it should "accept array creations" in {
    parseStatements("new int[5]; new int[a][][]; new a[c-(d)][];") should succeedParsingWith(expressionsAST(
      NewArray(TypeBasic("int"), IntLiteral("5"), 0),
      NewArray(TypeBasic("int"), Ident("a"), 2),
      NewArray(TypeBasic("a"), Apply("-", List(Ident("c"), Ident("d")), isOperator=true), 1)
    ))
  }

  it should "accept field accesses and method calls" in {
    parseStatements("a.b.c;a.b.c();a[b].c.c()[d];") should succeedParsingWith(expressionsAST(
      Select(Select(Ident("a"), "b"), "c"),
      Apply("c", List(Select(Ident("a"), "b"))),
      Apply("[]", List(
        Apply("c", List(
          Select(
            Apply("[]", List(
              Ident("a"),
              Ident("b")
            ), isOperator=true),
            "c"
          )
        )),
        Ident("d")
      ), true)
    ))
  }

  it should "accept long chains of field accesses" in {
    parseStatements(repeat("a.", 10000) + "b;") should succeedParsing()
  }

  it should "accept array access into new arrays" in {
    parseStatements("new array[10][][1];") should succeedParsingWith(expressionsAST(
      Apply("[]", List(NewArray(TypeBasic("array"), IntLiteral("10"), 1), IntLiteral("1")), isOperator=true)
    ))
  }

  /* negative test cases */

  it should "reject arbitrary expressions in member accesses" in {
    val parser = parseStatements("a.(b+c);")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject a class declaration without class name" in {
    val parser = parseProgram("class { }")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject a program with a premature EOF" in {
    val parser = parseProgram("class")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe a [Parser.UnexpectedTokenError]
    parser.findings.head.asInstanceOf[Parser.UnexpectedTokenError].token.data shouldBe TokenData.EOF
    parser.findings.head.pos.column shouldBe 6  // the char after "class"
  }

  it should "reject a program that declares variables inside non-block conditional scope" in {
    val parser = new Parser(new Lexer("class a { public void foo ( ) { if ( a ) int i ; } }").result)
    parser.result
    parser.success shouldBe false
    parser.findings.head shouldBe a [Parser.UnexpectedTokenError]
    parser.findings.head.pos.column shouldBe 42  // beginning of int
  }

  it should "reject an attept to create an array of something other than a basic type" in {
    val parser = parseStatements("new 3[4];")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject invalid main methods" in {
    val tests = List("public static void main(int[] args) {}", "public static void main(String args) {}",
      "public static void main(MyClass args) {}", "public static void main(MyClass2 args) {}")
    all(tests.map(p => parseProgram("class Test {" + p + "}"))) shouldNot succeedParsing()
  }

  it should "reject invalid 'new' expressions" in {
    val tests = List("new 3;", "new a[][6];", "new;", "new class()", "new MyClass;", "new MyClass(3);")
    all(tests.map(parseStatements)) shouldNot succeedParsing()
  }

  it should "reject invalid field declarations" in {
    val tests = List("public 3;", "public int;", "public void x(3, 4);", "public void[3] foo;",
      "public int a, b;")
    all(tests.map(p => parseProgram("class Test{" + p + "}"))) shouldNot succeedParsing()
  }
}
