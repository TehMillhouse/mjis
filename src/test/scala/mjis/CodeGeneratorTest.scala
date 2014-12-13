package mjis

import firm.{Firm, Backend, Mode}
import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.opt._
import org.scalatest._

class CodeGeneratorTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Firm.init()

    // tell FIRM we want to output amd64 code
    val modeP = Mode.createReferenceMode(
      "P64", Mode.Arithmetic.TwosComplement, 64, 64)
    Mode.setDefaultModeP(modeP)
    Backend.option("isa=amd64")
  }

  after {
    Firm.finish()
  }

  def template(code: String) =
    s"""  .text
      |
      |$code
      |
      |__main:
      |.L9000:
      |.L9001:
      |  ret
      |""".replace("  ", "\t").stripMargin

  "The code generator" should "generate code for a simple method" in {
    fromMembers("public int foo() { return 42; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movl $42, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for parameters" in {
    fromMembers("public int foo(int x) { return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for an arithmetic expression" in {
    fromMembers("public int foo(int x) { return x + 3; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  leal 3(%REG0{4}), %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for a more complex arithmetic expression" in {
    fromMembers("public int foo(int x, int y) { return (x + 3) * (y + 4); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |  movl %edx, %REG1{4}
        |.L0:
        |  leal 3(%REG0{4}), %REG2{4}
        |  leal 4(%REG1{4}), %REG3{4}
        |  movl %REG3{4}, %eax
        |  mull %REG2{4}
        |  movl %eax, %REG4{4}
        |  movl %REG4{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for array loads" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[i]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movl (%REG0{8},%REG1{8},4), %REG2{4}
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for nested array loads" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[xs[i]]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}   # xs
        |  movl %edx, %REG1{4}   # i
        |.L0:
        |  movl (%REG0{8},%REG1{8},4), %REG2{4}
        |  movl (%REG0{8},%REG2{8},4), %REG3{4}
        |  movl %REG3{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for array stores" in {
    fromMembers("public void foo(int[] xs, int i, int j) { xs[i] = j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |  movl %ecx, %REG2{4}
        |.L0:
        |  movl %REG2{4}, (%REG0{8},%REG1{8},4)
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads" in {
    fromMembers("public int i; public int j; public int foo(Test t) { return t.j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |.L0:
        |  movl 4(%REG0{8}), %REG2{4}
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate something for null loads" in {
    fromMembers("public int i; public int j; public int foo() { Test t = null; return t.j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $4, %REG0{8}
        |  movl (%REG0{8}), %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate something for null stores" in {
    fromMembers("public void foo() { Test[] t = null; t[1] = null; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $8, %REG0{8}
        |  movq $0, (%REG0{8})
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { return this.foo().t; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0{8}
        |.L0:
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1{8}
        |  movq (%REG1{8}), %REG2{8}
        |  movq %REG2{8}, %rax
        |.L1:
        |  ret"""),
      excludedOptimizations = Set(Inlining))
  }

  it should "generate code for member stores" in {
    fromMembers("public int i; public int j; public void foo(Test t, int i) { t.j = i; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movl %REG1{4}, 4(%REG0{8})
        |.L1:
        |  ret"""))
  }

  it should "generate code for member stores when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { this.foo().t = null; return this; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0{8}
        |.L0:
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1{8}
        |  movq $0, (%REG1{8})
        |  movq %REG0{8}, %rax
        |.L1:
        |  ret"""),
      excludedOptimizations = Set(Inlining))
  }

  it should "generate code for a method call" in {
    fromMembers("public int foo(int x) { return x; } public int bar() { return foo(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |.L1:
        |  ret
        |
        |_4Test_bar:
        |  movq %rdi, %REG2{8}
        |.L2:
        |  movq %REG2{8}, %rdi
        |  movl $42, %esi
        |  call _4Test_foo
        |  movl %eax, %REG3{4}
        |  movl %REG3{4}, %eax
        |.L3:
        |  ret"""),
      excludedOptimizations = Set(Inlining))
  }

  it should "generate code for System.out.println" in {
    fromMembers("public void foo() { System.out.println(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movl $42, %edi
        |  call System_out_println
        |.L1:
        |  ret"""))
  }

  it should "generate code for calloc" in {
    fromMembers("public Test t; public void foo() { new Test().foo(); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movl $1, %edi
        |  movl $8, %esi       # rsi doesn't need to be saved
        |  call calloc
        |  movq %rax, %REG0{8}
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |.L1:
        |  ret"""),
    excludedOptimizations = Set(Inlining))
  }

  it should "generate code for Phis" in {
    fromMembers("public int foo(int argI, boolean argB) { if (argB) argI = 0; return argI; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG1{4}  # argI
        |  movb %dl, %REG0{1}  # argB
        |.L0:
        |  cmpb $1, %REG0{1}
        |  je .L2
        |.L1:
        |  jmp .L3
        |.L2:
        |.L3:
        |  phi[%REG1{4}, $0] -> %REG2{4}
        |  movl %REG2{4}, %eax
        |.L4:
        |  ret"""))
  }

  it should "generate code for comparisons" in {
    fromMembers("public int foo(int argI) { if (argI > 0) return 1; else return 0; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  cmpl $0, %REG0{4}
        |  jg .L2
        |.L1:
        |  movl $0, %eax
        |  jmp .L3
        |.L2:
        |  movl $1, %eax
        |.L3:
        |  ret"""),
    excludedOptimizations = Set(Inlining))
  }

  it should "generate good loop code" in {
    fromMembers(
      """public boolean foo() {
        |  while (foo());
        |  while (!foo());
        |  return true;
        |}""".stripMargin) should succeedGeneratingCodeWith(template(
      """_4Test_foo:
      |  movq %rdi, %REG0{8}
      |.L0:
      |  jmp .L2
      |.L1:
      |.L2:
      |  movq %REG0{8}, %rdi
      |  call _4Test_foo
      |  movb %al, %REG1{1}
      |  cmpb $1, %REG1{1}
      |  je .L1
      |.L3:
      |  jmp .L5
      |.L4:
      |.L5:
      |  movq %REG0{8}, %rdi
      |  call _4Test_foo
      |  movb %al, %REG2{1}
      |  cmpb $1, %REG2{1}
      |  jne .L4
      |.L6:
      |  movb $1, %al
      |.L7:
      |  ret"""),
      excludedOptimizations = Set(Inlining))
  }

  it should "generate code for a trivial infinite loop" in {
    fromMembers("public void foo() { while (true); }") should succeedGeneratingCodeWith(template(
    """_4Test_foo:
      |.L0:
      |.L1:
      |.L2:
      |  jmp .L1"""))
  }

  it should "generate code for division by a constant" in {
    fromMembers("public int foo(int x) { return x / 2; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |  cdq
        |  movl $2, %REG1{4}
        |  idivl %REG1{4}
        |  movl %eax, %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate code for modulo division by a constant" in {
    fromMembers("public int foo(int x) { return x % 2; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |  cdq
        |  movl $2, %REG1{4}
        |  idivl %REG1{4}
        |  movl %edx, %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate correct lea instructions" in {
    fromMembers("public int foo(int x, int y) { return x+3+8*y; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |  movl %edx, %REG1{4}
        |.L0:
        |  leal 3(%REG0{4},%REG1{4},8), %REG2{4}
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }
}
