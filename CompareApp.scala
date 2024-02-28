// receive 2 file paths as input and compare the files, exit with status code 0 if there was no breaking change, 1 otherwise
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
object CompareApp extends App {
  private def writeToOutput(
      values: Map[String, String] = Map.empty[String, String],
      githubOutput: String
  ): Unit = {
    Files.write(
      Paths.get(githubOutput),
      values
        .map { case (key, value) => s"$key=$value" }
        .mkString("\n")
        .getBytes(
          StandardCharsets.UTF_8
        )
    )
  }

  val filesList =
    sys.env("INPUT_FILES").split(",").filterNot(_.isBlank())
    
  // read environment variable GITHUB_OUTPUT
  val githubOutput = sys.env("GITHUB_OUTPUT")
  val result = filesList.map(file => (file, file + ".prev")).map {
    case (oldFile, newFile) => {
      val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
      val newFileParsed = FileParser.fromPathToClassDef(newFile)

      val compared =
        BreakingChangeDetector.detectBreakingChange(
          oldFileParsed,
          newFileParsed
        )

      if (compared.find(_.isBreakingChange).isEmpty) {
        val output = "No breaking change detected"
        // write false to environment variable GITHUB_OUTPUT
        (false, output, oldFile)
      } else {
        val output =
          List(
            "Breaking change detected",
            compared.filter(_.isBreakingChange).map(_.toString)
          ).mkString("\n")
        (true, output, oldFile)

      }
    }
  }
  val finalResult = result.map(_._1).foldLeft(false)(_ || _)
  // listOfFilesThatBreakChange
  val finalOutput = result.map(_._3).mkString(",")

  writeToOutput(
    Map(
      "result" -> finalResult.toString,
      "log" -> finalOutput
    ),
    githubOutput
  )

  println("result=" + finalResult.toString)
  println("log=" + finalOutput)
  System.exit(0)
}
