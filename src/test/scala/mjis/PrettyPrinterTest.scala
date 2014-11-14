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

  "The pretty-printer" should "collapse empty classes" in {
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

  it should "omit an empty else" in {
    prettyPrintProgram(
        """class HelloWorld
           |{
           |  public int bar(int a, int b) {
           |    if (true) {
           |      int i;
           |    } else ;
           |  }
           |}""".stripMargin) should succeedPrettyPrintingWith(
        """class HelloWorld {
           |	public int bar(int a, int b) {
           |		if (true) {
           |			int i;
           |		}
           |	}
           |}
           |""".stripMargin)
  }

  it should "properly prettyprint the example from the sheet" in {
    prettyPrintProgram(
        """class HelloWorld
           |{
           |  public int c;
           |  public boolean[] array;
           |  public static /* blabla */ void main(String[] args)
           |  { System.out.println( (43110 + 0) );
           |    boolean b = true && (!false);
           |    if (23+19 == (42+0)*1)
           |      b = (0 < 1);
           |    else if (!array[2+2]) {
           |      int x = 0;;
           |      x = x+1;
           |    } else {
           |      new HelloWorld().bar(42+0*1, -1);
           |    }
           |  }
           |  public int bar(int a, int b) { return c = (a+b); }
           |}""".stripMargin) should succeedPrettyPrintingWith(
        """class HelloWorld {
           |	public int bar(int a, int b) {
           |		return c = (a + b);
           |	}
           |	public static void main(String[] args) {
           |		(System.out).println(43110 + 0);
           |		boolean b = true && (!false);
           |		if ((23 + 19) == ((42 + 0) * 1))
           |			b = (0 < 1);
           |		else if (!(array[2 + 2])) {
           |			int x = 0;
           |			x = (x + 1);
           |		} else {
           |			(new HelloWorld()).bar(42 + (0 * 1), -1);
           |		}
           |	}
           |	public boolean[] array;
           |	public int c;
           |}
           |""".stripMargin)
  }

  it should "pretty-print many fields, main methods and methods" in {
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
        |}
        |""".stripMargin)
  }
}
