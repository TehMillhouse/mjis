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

  def template(code: String, mainMethod: Boolean = true) =
    (s"""  .text
      |
      |$code
      |""" + (if (mainMethod) """
      |__main:
      |.L9000:
      |.L9001:
      |  ret
      |""" else "")).replace("  ", "\t").stripMargin

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

  it should "generate code for slightly simpler arithmetic expression" in {
    fromMembers("public int foo(int x, int y) { return x + 16 * y; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movl %REG1{4}, %REG2{4}
        |  shll $4, %REG2{4}
        |  leal (%REG0{4},%REG2{4}), %REG3{4}
        |  movl %REG3{4}, %eax
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

  it should "generate code for array loads, minding the non-existent sign extension" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[i+3]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |.L0:
        |  leal 3(%REG1{4}), %REG2{4}
        |  movl (%REG0{8},%REG2{8},4), %REG3{4}
        |  movl %REG3{4}, %eax
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
        |  movq $0, %REG0{8}
        |  movl (%REG0{8}), %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate something for Unknown loads" in {
    fromMembers("public int foo() { int[] a; return a[0];} ") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $0, %REG0{8}
        |  movl (%REG0{8}), %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate something for non-constant null loads" in {
    fromMembers("public int foo(int i) { int[] a = null; return a[i]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $0, %REG0{8}
        |  movl (%REG0{8}), %REG1{4}
        |  movl %REG1{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "generate something for null stores" in {
    fromMembers("public void foo() { Test[] t = null; t[1] = null; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $0, %REG0{8}
        |  movq $0, (%REG0{8})
        |.L1:
        |  ret"""))
  }

  it should "generate something for Unknown stores" in {
    fromMembers("public void foo() { int[] a; a[0] = 42;} ") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $0, %REG0{8}
        |  movl $42, (%REG0{8})
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

  it should "properly inline a trivial call" in {
    fromMembers(
      """public static void main(String[] args) { new Test().foo(); }
        |public void foo() { System.out.println(42); }""".stripMargin, mainMethod = false) should succeedGeneratingCodeWith(template(
    """__main:
      |.L6:
      |	movl $1, %edi
      |	movl $0, %esi
      |	call calloc
      |	movq %rax, %REG0{8}
      |.L8:
      |	movl $42, %edi
      |	call System_out_println
      |.L9:
      |.L10:
      |	ret
      |
      |_4Test_foo:
      |.L0:
      |  movl $42, %edi
      |  call System_out_println
      |.L3:
      |  ret""", mainMethod = false), excludedOptimizations = Set(UnusedParameterElimination, PureFunctionCallElimination))
  }

  it should "properly inline recursive calls" in {
    fromMembers(
      """
        |public static void main(String[] args) { new Test().foo(); }
        |public boolean foo() {
        |  if (foo()) { System.out.println(42);
        |  }
        |  return true;
        |}
      """.stripMargin, mainMethod=false) should succeedGeneratingCodeWith(template(
      """__main:
         |.L0:
         |	movl $1, %edi
         |	movl $0, %esi
         |	call calloc
         |	movq %rax, %REG0{8}
         |	movq %REG0{8}, %rdi
         |	call _4Test_foo
         |	movb %al, %REG1{1}
         |.L1:
         |	ret
         |
         |_4Test_foo:
         |	movq %rdi, %REG2{8}
         |.L2:
         |.L3:
         |	movq %REG2{8}, %rdi
         |	call _4Test_foo
         |	movb %al, %REG3{1}
         |.L4:
         |	cmpb $0, %REG3{1}
         |	jne .L6
         |.L5:
         |	jmp .L7
         |.L6:
         |	movl $42, %edi
         |	call System_out_println
         |.L7:
         |.L8:
         |.L9:
         |	movl $42, %edi
         |	call System_out_println
         |.L10:
         |	movb $255, %al
         |.L11:
         |  ret""", mainMethod=false), excludedOptimizations = Set(UnusedParameterElimination, PureFunctionCallElimination))

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
    fromMembers("public int foo(int arg1, int arg2) { if (arg2 == 5) arg1 = 0; return arg1; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}  # arg1
        |  movl %edx, %REG1{4}  # arg2
        |.L0:
        |  cmpl $5, %REG1{4}
        |  je .L2
        |.L1:
        |  jmp .L3
        |.L2:
        |.L3:
        |  phi[%REG0{4}, $0] -> %REG2{4}
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
      |  cmpb $0, %REG1{1}
      |  jne .L1
      |.L3:
      |  jmp .L5
      |.L4:
      |.L5:
      |  movq %REG0{8}, %rdi
      |  call _4Test_foo
      |  movb %al, %REG2{1}
      |  cmpb $0, %REG2{1}
      |  je .L4
      |.L6:
      |  movb $255, %al
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

  it should "generate code for division by zero" in {
    fromMembers("public int foo(int dividend) { return dividend / 0; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |  movl $0, %REG1{4}
        |  idivl %REG1{4}
        |  movl %eax, %REG1{4}
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

  it should "generate conditional move instructions for Mux nodes" in {
    fromMembers("public int foo(int j) { int x; if (j % 2 == 0) x = 1; else x = 2; return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %REG1{4}
        |  andl $1, %REG1{4}
        |  movl $1, %REG2{4}
        |  movl $2, %REG3{4}
        |  cmpl $0, %REG1{4}
        |  cmovel %REG2{4}, %REG3{4}
        |  movl %REG3{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "avoid having to load a constant when generating conditional moves (1)" in {
    fromMembers("public int foo(int j) { int x; if (j % 2 == 0) x = 0; else x = 1; return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %REG1{4}
        |  andl $1, %REG1{4}
        |  movl $1, %REG2{4}
        |  cmpl $0, %REG1{4}
        |  cmovel %REG1{4}, %REG2{4}
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "avoid having to load a constant when generating conditional moves (2)" in {
    fromMembers("public int foo(int j) { int x; if (j % 2 != 0) x = 1; else x = 0; return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %REG1{4}
        |  andl $1, %REG1{4}
        |  movl $1, %REG2{4}
        |  cmpl $0, %REG1{4}
        |  cmovel %REG1{4}, %REG2{4}
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }

  it should "not attempt to generate size-extending moves" in {
    fromMembers("public int foo(boolean b) { int ret; if (b) ret = 1; else ret = 0; return ret; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movb %sil, %REG0{1}
        |.L0:
        |  movl $1, %REG1{4}
        |  movl $0, %REG2{4}
        |  cmpb $0, %REG0{1}
        |  cmovnel %REG1{4}, %REG2{4}   # should not do "cmov %REG0{1}, %REG2{4}" heres
        |  movl %REG2{4}, %eax
        |.L1:
        |  ret"""))
  }
}
