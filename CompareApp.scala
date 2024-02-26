// receive 2 file paths as input and compare the files, exit with status code 0 if there was no breaking change, 1 otherwise
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
object CompareApp extends App {
  private def writeResult(result: Boolean, githubOutput: String): Unit = {
    Files.write(Paths.get(githubOutput), s"result=${result}".getBytes(StandardCharsets.UTF_8))
  }
  // get path from environment variable
  val oldFile = sys.env("INPUT_OLD_FILE")

  // get path from environment variable
  val newFile = sys.env("INPUT_NEW_FILE")

  // read environment variable GITHUB_OUTPUT
  val githubOutput = sys.env("GITHUB_OUTPUT")

  val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
  val newFileParsed = FileParser.fromPathToClassDef(newFile)

  val compared =
    BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)

  if (compared.find(_.isBreakingChange).isEmpty) {
    val output = "No breaking change detected"
    // write false to environment variable GITHUB_OUTPUT
    writeResult(false, githubOutput)
    println(output)
  } else {
    val output =
      List(
        "Breaking change detected",
        compared.filter(_.isBreakingChange).map(_.toString)
      ).mkString("\n")
    writeResult(true, githubOutput)
    println(output)
    }
    System.exit(0)
}
