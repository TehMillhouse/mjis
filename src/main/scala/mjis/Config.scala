package mjis

import java.nio.file.{Paths, Path}

case class Config(stopAfter: String = "",
                  file: Option[Path] = null,
                  useFirmBackend: Boolean = false,
                  outFile: Path = Paths.get("a.out").toAbsolutePath) {
  def asmOutFile: Path = Paths.get(outFile.toString + ".s")
}
