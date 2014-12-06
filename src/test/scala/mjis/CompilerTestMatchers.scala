package mjis

import firm.Graph
import org.scalatest.Assertions
import org.scalatest.matchers.{ MatchResult, Matcher }
import mjis.ast._
import System.{ lineSeparator => n }
import java.io._
import scala.collection.JavaConversions._
import scala.io.Source

import scala.reflect.ClassTag

trait CompilerTestMatchers {

  class AnalysisPhaseSuccessMatcher[P <: AnalysisPhase[_]: ClassTag]() extends Matcher[String] {
    def mkFailureMessage(phase: P): Option[String] = None
    def apply(input: String) = {
      val result = Compiler.exec[P](new StringReader(input))
      val failureMessage = result match {
        case Left(phase) => mkFailureMessage(phase)
        case _ => Some(s"Failed, expected to succeed. Findings:$n${result.right.get.mkString(n)}")
      }

      MatchResult(failureMessage.isEmpty, failureMessage.getOrElse(""), "Succeeded, expected to fail")
    }
  }

  class AnalysisPhaseSuccessWithMatcher[O, P <: AnalysisPhase[O]: ClassTag](expected: O) extends AnalysisPhaseSuccessMatcher[P] {
    override def mkFailureMessage(phase: P): Option[String] =
      if (expected == phase.result) None
      else Some(s"Expected: $expected${n}Computed: ${phase.result}")
  }

  class AnalysisPhaseFailureWithMatcher[P <: AnalysisPhase[_]: ClassTag](expectedFinding: Finding) extends Matcher[String] {
    def apply(input: String) = {
      val result = Compiler.exec[P](new StringReader(input))
      val failureMessage = result match {
        case Right(findings) =>
          if (expectedFinding == findings.head) None
          else Some(s"Expected ${expectedFinding},$n     got ${findings.head}")
        case _ => Some(s"Succeeded, expected to fail.")
      }

      MatchResult(failureMessage.isEmpty, failureMessage.getOrElse(""), "Why would you negate this??")
    }
  }

  class PrettyPrinterSuccessMatcher(expectedString: String) extends AnalysisPhaseSuccessMatcher[Parser]() {
    override def mkFailureMessage(parser: Parser) = {
      val out = new StringWriter()
      parser.dumpResult(new BufferedWriter(out))
      if (out.toString != expectedString)
        Some(s"Expected:$n'$expectedString'${n}Computed:$n'${out.toString}'")
      else {
        // second pass to make sure round-tripping works and pretty printer is idempotent
        Compiler.exec[Parser](new StringReader(out.toString)) match {
          case Left(parser2) =>
            val out2 = new StringWriter()
            parser2.dumpResult(new BufferedWriter(out2))
            if (out2.toString == expectedString) None
            else Some(s"Round-tripping failed:$n${out2.toString}${n}Original String:$n${out.toString}$n")
          case Right(findings) => Some(s"Round-tripping failed:$n$findings")
        }
      }
    }
  }

  class FirmConstructorSuccessMatcher(expectedGraphs: List[Graph]) extends Matcher[String] {
    def apply(input: String) = {
      val expectedPrefix = FirmGraphTestHelper.ExpectedPrefix
      val expectedGraphsMap = (expectedGraphs map { g => {
        assert(g.getEntity.getName.startsWith(expectedPrefix), s"Expected graph names must start with '$expectedPrefix'")
        g.getEntity.getName.drop(expectedPrefix.length) -> g
      }}).toMap

      var success = true
      var failureMessage: String = ""

      val analysisResult = Compiler.exec[Typer](new StringReader(input))
      if (analysisResult.isRight)
        Assertions.fail(s"Analysis failed, expected to succeed. Findings:$n${analysisResult.right.get.mkString(n)}")

      new FirmConstructor(analysisResult.left.get.result).result
      val actualGraphsMap = firm.Program.getGraphs.
        filter(g => !g.getEntity.getName.startsWith(expectedPrefix)).
        map(g => g.getEntity.getName -> g).toMap
      if (actualGraphsMap.size != expectedGraphsMap.size) {
        success = false
        val expGraphs = expectedGraphsMap.keys.mkString(", ")
        val actGraphs = actualGraphsMap.keys.mkString(", ")
        failureMessage = s"Expected ${expectedGraphsMap.size} graph(s): $expGraphs, got ${actualGraphsMap.size}: $actGraphs"
      } else {
        expectedGraphsMap.foreach { case (name, expected) => actualGraphsMap.get(name) match {
          case None =>
            success = false
            failureMessage = s"Missing graph $name"
          case Some(actual) =>
            val error = FirmGraphTestHelper.isIsomorphic(expected, actual)
            if (!error.isEmpty) {
              success = false
              failureMessage = s"Graphs ${expected.getEntity.getName} and ${actual.getEntity.getName} " +
                s"were not isomorphic: $error"
            }
        }}
      }

      MatchResult(
        success, failureMessage, "Expected FIRM construction to fail, but it succeeded"
      )
    }
  }

  class FirmGraphIsomorphismMatcher(expectedGraph: Graph) extends Matcher[Graph] {
    override def apply(left: Graph): MatchResult = {
      val result = FirmGraphTestHelper.isIsomorphic(left, expectedGraph)
      MatchResult(
        result.isEmpty,
        s"Expected the graphs to be isomorphic, but they weren't: $result",
        "Expected the graphs to be not isomorphic, but they were")
    }
  }


  class IntegrationTestMatcher() extends Matcher[String] {

    def compileCmd(path: String) = "java -cp target/scala-2.11/classes:" +
      s"target/scala-2.11/mjis-assembly-0.1-SNAPSHOT-deps.jar:lib/jfirm.jar:lib/jna.jar mjis/CLIMain $path"

    def apply(path: String) = {
      val pb = new ProcessBuilder(compileCmd(path).split(' ').toList)
      val env = pb.environment()
      env.put("LD_LIBRARY_PATH", env.get("LD_LIBRARY_PATH") + ":lib")
      val compilerProcess = pb.start()
      val stderr = new BufferedReader(new InputStreamReader(compilerProcess.getErrorStream))
      compilerProcess.waitFor()
      val stream = Stream.continually(stderr.readLine()).takeWhile(_ != null)
      if (compilerProcess.exitValue != 0 || stream.nonEmpty)
        Assertions.fail(stream.mkString("\n"))

      val testProcess = Runtime.getRuntime.exec("./a.out")
      val stdout = new BufferedReader(new InputStreamReader(testProcess.getInputStream))
      val checkString = Source.fromFile(path.stripSuffix("mj") + "check").mkString.stripLineEnd
      testProcess.waitFor()
      val outputString = Stream.continually(stdout.readLine()).takeWhile(_ != null).mkString("\n").stripLineEnd

      MatchResult(checkString == outputString,
        s"""Expected output is not equal to actual output
          |test file: $path
          |expected:
          |$checkString
          |actual:
          |$outputString""".stripMargin,
        "Expected output is equal to actual output\n")
    }
  }

  def succeedLexing() = new AnalysisPhaseSuccessMatcher[Lexer]()
  def succeedParsing() = new AnalysisPhaseSuccessMatcher[Parser]()
  def succeedParsingWith(expectedAST: Program) = new AnalysisPhaseSuccessWithMatcher[Program, Parser](expectedAST)
  def failParsingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Parser](expectedFinding)
  def succeedPrettyPrintingWith(expectedString: String) = new PrettyPrinterSuccessMatcher(expectedString)
  def succeedTyping = new AnalysisPhaseSuccessMatcher[Typer]()
  def failTypingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Typer](expectedFinding)
  def succeedNaming() = new AnalysisPhaseSuccessMatcher[Namer]()
  def failNamingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Namer](expectedFinding)
  def succeedFirmConstructingWith(expectedGraphs: List[Graph]) = new FirmConstructorSuccessMatcher(expectedGraphs)
  def passIntegrationTest() = new IntegrationTestMatcher()
  def beIsomorphicTo(expectedGraph: Graph) = new FirmGraphIsomorphismMatcher(expectedGraph)
}

object CompilerTestMatchers extends CompilerTestMatchers
