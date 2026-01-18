// receive 2 file paths as input and compare the files, exit with status code 0 if there was no breaking change, 1 otherwise
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import BreakingChangeDetector._
import BreakingChangeDetector.CompareSummary._
import upickle.default._

object CompareApp {
  implicit val compareSummaryRw: ReadWriter[CompareSummary] = rw

  def main(args: Array[String]): Unit = {
    def writeToOutput(
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
      case (newFile, oldFile) => {
        val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
        val newFileParsed = FileParser.fromPathToClassDef(newFile)

        val compared =
          detectBreakingChange(
            oldFileParsed,
            newFileParsed
          )

        if (compared.find(_.isBreakingChange).isEmpty) {
          val output = List.empty[CompareSummary]
          // write false to environment variable GITHUB_OUTPUT
          (false, output, newFile)
        } else {
          val output = compared.filter(_.isBreakingChange)
          println(output)
          (true, output, newFile)

        }
      }
    }
    val finalResult = result.map(_._1).foldLeft(false)(_ || _)
    // listOfFilesThatBreakChange
    val finalOutput = result.filter(_._1).map(_._3).mkString(",")
    val finalRaw = result.filter(_._1).map(res => (res._3, res._2)).toMap
    val finalJson = write(finalRaw)
    writeToOutput(
      Map(
        "result" -> finalResult.toString,
        "log" -> finalOutput,
        "json" -> finalJson
      ),
      githubOutput
    )
    println("json=" + finalJson)
    println("result=" + finalResult.toString)
    println("log=" + finalOutput)
    System.exit(0)
  }
}
