package mjis

import org.scalatest._
import CompilerTestMatchers._
import util.PrettyPrinter

class PrettyPrinterTest extends FlatSpec with Matchers with Inspectors {

  def prettyPrintProgram(program: String) = {
    val lexer = new Lexer(program)
    val parser = new Parser(lexer.result)
    // The lexer result is only traversable once, so evaluate it only after the parser has finished.
    parser.result
    lexer should succeedLexing()
    parser should succeedParsing()
    parser
  }

  it should "collapse empty classes" in {
    prettyPrintProgram(
        """class A {}""") should succeedPrettyPrintingWith(
            """class A { }
               |""".stripMargin)
  }
  
  it should "sort members correctly" in {
    prettyPrintProgram(
        """class A {
           |  public void c() {}
           |  public int a;
           |  public void b() {}
           |  public int b;
           |}""".stripMargin) should succeedPrettyPrintingWith(
               """class A {
                  |	public void b() { }
                  |	public void c() { }
                  |	public int a;
                  |	public int b;
                  |}
                  |""".stripMargin)
  }
  
  it should "collapse else after a block" in {
    prettyPrintProgram(
        """class A {
           |  public void a() {
           |    if (true) {
           |      return;
           |    }
           |    else {}
           |  }
           |}""".stripMargin) should succeedPrettyPrintingWith(
               """class A {
                  |	public void a() {
                  |		if (true) {
                  |			return;
                  |		} else { }
                  |	}
                  |}
                  |""".stripMargin)
  }
  
  // main is currently not being handled correctly
  ignore should "pretty-print many fields, main methods and methods" in {
    prettyPrintProgram(
      """class C {
        |public int x;
        |public static void main(String[] args) {}
        |public int z(int j, A b) {}
        |public X[][] foo(Y[][][] y) {}
        |}""".stripMargin) should succeedPrettyPrintingWith(
            """class C {
               |	public X[][] foo(Y[][][] y) { }
               |	public static void main(String[] args) { }
               |	public int z(int j, A b) { }
               |	public int x;
               |}""".stripMargin)
  }
}
