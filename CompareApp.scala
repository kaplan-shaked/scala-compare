// receive 2 file paths as input and compare the files, exit with status code 0 if there was no breaking change, 1 otherwise
object CompareApp extends App {

  val oldFile = args(0)
  val newFile = args(1)

  val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
  val newFileParsed = FileParser.fromPathToClassDef(newFile)

  val compared =
    BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)

  if (compared.find(_.isBreakingChange).isEmpty) {
    println("No breaking change detected")
    System.exit(0)
  } else {
    println("Breaking change detected")
    compared.filter(_.isBreakingChange).foreach(println)
    System.exit(1)
  }

}
