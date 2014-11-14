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

    if (config.stopAfter == "lexer") {
      val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"))
      lexer.dumpResult(out)
    } else if (config.stopAfter == "parser") {
      val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"))
      parser.dumpResult(out)
    } else {
      parser.result // something something
    }

    lexer.findings.foreach(System.err.println)
    parser.findings.map(System.err.println)
    lexer.success && parser.success
  }
}
