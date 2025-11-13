class BreakingChangeDetectorTest extends munit.FunSuite {
  test("V1 without breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("v1.scala_test")
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
      .getResource("v1.scala_test")
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
      .getResource("v1.scala_test")
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

  test("compare V4 with V4") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V4.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V4.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }

  test("compare V5 breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V5.scala_test")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V5-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).nonEmpty
    )
  }

  test("compare V6 Not breaking change") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V6.scala_test.prev")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V6.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }

  test(
    "Shoud dectect breaking change when default value is dropped from a field"
  ) {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V7.scala_test.prev")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V7.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).nonEmpty
    )
  }
  test("Shoud dectect breaking change when default value is added to a field") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V8.scala_test.prev")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V8.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).nonEmpty
    )
  }

  test("When a new field is added with default value it should be okay") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V9.scala_test.prev")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("V9.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).isEmpty
    )
  }

  test("Should detect ReaderWriter changed to ResourceWriter, ResourceReader") {
    def runWithFiles(olfFileName: String, newFileName: String) = {
      val oldFile = Thread
        .currentThread()
        .getContextClassLoader
        .getResource("V10.scala_test.prev")
        .getPath
      val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
      val newFile = Thread
        .currentThread()
        .getContextClassLoader
        .getResource("V10.scala_test")
        .getPath
      val newFileParsed = FileParser.fromPathToClassDef(newFile)
      val compared =
        BreakingChangeDetector.detectBreakingChange(
          oldFileParsed,
          newFileParsed
        )
      println(compared)
      assert(
        compared.find(_.isBreakingChange).nonEmpty
      )
    }

    runWithFiles("V10.scala_test.prev", "V10.scala_test")
    runWithFiles("V10.scala_test", "V10.scala_test.prev")
  }

  test("Should detect breaking change with implicit ReadWriter - added field without default") {
    val oldFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/implicit-breaking-change.scala_test.prev")
      .getPath
    val oldFileParsed = FileParser.fromPathToClassDef(oldFile)
    val newFile = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/implicit-breaking-change.scala_test")
      .getPath
    val newFileParsed = FileParser.fromPathToClassDef(newFile)
    val compared =
      BreakingChangeDetector.detectBreakingChange(oldFileParsed, newFileParsed)
    println(compared)
    assert(
      compared.find(_.isBreakingChange).nonEmpty
    )
    val breakingChange = compared.find(_.isBreakingChange).get
    assert(breakingChange.addedFieldsWithoutDefaultValues.contains("email"))
  }
}
