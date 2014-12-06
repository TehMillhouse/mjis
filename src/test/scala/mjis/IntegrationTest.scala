package mjis

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}
import CompilerTestMatchers.passIntegrationTest

class IntegrationTest extends FlatSpec with Matchers with BeforeAndAfter {

  val TestDir = "mj-test/run/"

  "The integration tests" should "produce correct output" in {
    Runtime.getRuntime.exec("sbt compile").waitFor()
    val testFiles = new java.io.File(TestDir).listFiles.filter(_.getName.endsWith(".mj"))
    for (file <- testFiles) {
      println(s"Compiling and matching $file")
      file.getPath should passIntegrationTest()
    }
  }
}
