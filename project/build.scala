import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object MJIS extends Build {
  val shellScript = Seq("#!/usr/bin/env sh",
    """LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(dirname $0)/../../lib exec java -jar "$0" "$@" """)

  val binary = TaskKey[Unit]("binary", "creates an executable polyglot ELF/jar binary")
  (binary in Compile) <<= (binary in Compile) dependsOn (assembly in Compile)

  val _jarName = "run.sh"
  val buildDir = "target/scala-2.11"

  lazy val root =
    project
      .in(file("."))
      .settings(assemblySettings: _*)
      .settings(
        name := "mjis",
        jarName in assembly := _jarName,
        mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
            case PathList("META-INF") => MergeStrategy.discard
            case x => old(x)
          }
        },
        unmanagedResourceDirectories in Compile += baseDirectory.value / "lib" / "stdlib",
        mainClass in assembly := Some("mjis.CLIMain"),
        test in assembly := {},
        assemblyOption in assembly ~= { _.copy(prependShellScript = Some(shellScript)) },
        scalaVersion := "2.11.4",
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Xfuture"),
        libraryDependencies += "com.github.scopt" % "scopt_2.11" % "3.2.0",
        libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
        parallelExecution in test := false,
        binary in Compile <<= (assembly in Compile) map { _ =>
          import scala.sys.process._
          println("Creating ELF/jar file...")
          Seq("cp", "lib/libfirm.so", buildDir + "/libfirm.so").!!
          Seq("cat", buildDir + "/" + _jarName) #>> new File(buildDir + "/libfirm.so") !!
        }
      )

}
