class BreakingChangeDetectorTest extends munit.FunSuite {
  test("V1 without breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V1.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V1-not-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }
  test("V1 with breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V1.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V1-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).nonEmpty
    )
  }
  test("V2 without breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V2.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V2-not-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }
  test("V2 with breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V2.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V2-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }

  test("compare v1 with v2") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V1.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V2.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }
  
}
