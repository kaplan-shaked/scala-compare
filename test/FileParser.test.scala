class FileParserTest extends munit.FunSuite {
  test("should parse file correctly") {
    // read test.scala_test from resources
    val file = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("test.scala_test")
      .getPath
    val parsedFile = FileParser.fromPathToClassDef(file)
    assert(parsedFile.classes.length == 5)
    assert(parsedFile.classes(0).name == "TestClass1")
    // assert fields parse correctly
    assert(parsedFile.classes(0).fields(0).name == "field1")
    assert(parsedFile.classes(0).fields(0).tpe == "String")
    assert(parsedFile.classes(0).fields(0).default.isEmpty)
    assert(parsedFile.classes(0).fields(1).name == "field2")
    assert(parsedFile.classes(0).fields(1).tpe == "Int")
    assert(parsedFile.classes(0).fields(1).default.isEmpty)
    assert(parsedFile.classes(1).name == "TestClass2")
    assert(parsedFile.classes(1).fields(0).name == "field3")
    assert(parsedFile.classes(1).fields(0).tpe == "Double")
    assert(parsedFile.classes(1).fields(0).default.contains("0.0"))
    assert(parsedFile.classes(1).fields(1).name == "field4")
    assert(parsedFile.classes(1).fields(1).tpe == "Boolean")
    assert(parsedFile.classes(2).name == "TestClass3")
    assert(parsedFile.classes(2).fields(0).name == "field5")
    assert(parsedFile.classes(2).fields(0).tpe == "Long")
    assert(parsedFile.classes(2).fields(0).default.isEmpty)
    assert(parsedFile.classes(2).fields(1).name == "field6")
    assert(parsedFile.classes(2).fields(1).tpe == "Float")
    assert(parsedFile.classes(2).fields(1).default.isEmpty)
    assert(parsedFile.classes(3).name == "TestClass4")
    assert(parsedFile.classes(3).fields(0).name == "field7")
    assert(parsedFile.classes(3).fields(0).tpe == "String")
    assert(parsedFile.classes(3).fields(0).default.isEmpty)
    assert(parsedFile.classes(3).fields(1).name == "field8")
    assert(parsedFile.classes(3).fields(1).tpe == "Int")
    assert(parsedFile.classes(3).fields(1).default.isEmpty)
    assert(parsedFile.classes(4).name == "TestClass5")
    assert(parsedFile.classes(4).fields(0).name == "field9")
    assert(parsedFile.classes(4).fields(0).tpe == "Double")
    assert(parsedFile.classes(4).fields(0).default.isEmpty)
    assert(parsedFile.classes(4).fields(1).name == "field10")
    assert(parsedFile.classes(4).fields(1).tpe == "Boolean")
    assert(parsedFile.classes(4).fields(1).default.contains("true"))
  }

  test("can parse V3") {
    val file = Thread
        .currentThread()
        .getContextClassLoader
        .getResource("V3.scala_test")
        .getPath
      val parsedFile = FileParser.fromPathToClassDef(file)
      println(parsedFile)
  }
  test("can parse V4") {
    val file = Thread
        .currentThread()
        .getContextClassLoader
        .getResource("V4.scala_test")
        .getPath
      val parsedFile = FileParser.fromPathToClassDef(file)
      println(parsedFile)
  }
}
