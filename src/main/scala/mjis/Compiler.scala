package mjis

import java.io._
import java.nio.file.{Path}
import scala.collection.JavaConversions._

object Compiler {
  def compile(config: Config): Boolean = {
    // Concatenate input files
    val concatenatedInputStream = new SequenceInputStream((config.files map {
      path: Path =>
        if (path.toString() == "")
          new BufferedInputStream(System.in)
        else
          new BufferedInputStream(new FileInputStream(path.toFile))
    }).iterator)

    val lexer = new Lexer(new InputStreamReader(concatenatedInputStream, "ASCII"))
    val parser = new Parser(lexer.result)

    if (config.stopAfter != "") {
      val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"))
      config.stopAfter match {
        case "lexer" => lexer.dumpResult(out)
        case "parser" => parser.dumpResult(out)
      }
      out.close()
    } else
      parser.result // force evaluation

    lexer.findings.foreach(System.err.println)
    parser.findings.map(System.err.println)
    lexer.success && parser.success
  }
}
