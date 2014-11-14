package mjis

import org.scalatest.matchers.{ MatchResult, Matcher }
import mjis.ast.SyntaxTree
import System.{ lineSeparator => n }
import mjis.util.PrettyPrinter
import java.io.{StringWriter, BufferedWriter}

trait CompilerTestMatchers {

  class LexerSuccessMatcher() extends Matcher[Lexer] {
    def apply(lexer: Lexer) = {
      lexer.result.length
      MatchResult(
        lexer.success,
        "Lexing failed, expected it to succeed. Findings: " + System.lineSeparator() +
          lexer.findings.mkString(System.lineSeparator()),
        "Lexing succeeded, expected it to fail")
    }
  }

  class ParserSuccessMatcher(expectedAST: Option[SyntaxTree]) extends Matcher[Parser] {
    def apply(parser: Parser) = {
      val result = parser.result
      def findError: String =
        if (!parser.success) s"Findings:$n${parser.findings.mkString(n)}"
        else if (expectedAST.isDefined)
          s"$n  Expected AST: ${expectedAST.get}$n  Computed AST: ${result.get}$n"
        else
          ""
      MatchResult(
        parser.success && (expectedAST.isEmpty || result == expectedAST),
        "Parsing failed, expected it to succeed. " + findError,
        "Parsing succeeded, expected it to fail")
    }
  }

  class PrettyPrinterSuccessMatcher(expectedString: String) extends Matcher[Parser] {
    def apply(parser: Parser) = {
      val result = parser.result
      val stw = new StringWriter();
      val buf = new BufferedWriter(stw)
      new PrettyPrinter(buf).print(result.get)
      buf.flush()
      val prettyPrint = stw.toString()
      def findError: String = s"$n  Expected String:$n$expectedString$n  Computed String:$n$prettyPrint$n"
      val success = prettyPrint == expectedString
      val error = if (success) "" else findError

      MatchResult(
        success,
        s"Pretty printing failed, expected it to succeed.$error",
        "Parsing succeeded, expected it to fail")
    }
  }

  def succeedLexing() = new LexerSuccessMatcher()
  def succeedParsing() = new ParserSuccessMatcher(None)
  def succeedParsingWith(expectedAST: SyntaxTree) = new ParserSuccessMatcher(Some(expectedAST))
  def succeedPrettyPrintingWith(expectedString: String) = new PrettyPrinterSuccessMatcher(expectedString)

}

object CompilerTestMatchers extends CompilerTestMatchers
