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
  test("uses Scala3 dialect for sources in scala-3 directory") {
    val file = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/Scala3Class.scala_test")
      .getPath
    val parsedFile = FileParser.fromPathToClassDef(file)
    assert(parsedFile.classes.head.name == "Scala3Class")
    // Assert only first param list is treated as fields (using params should be ignored)
    assert(parsedFile.classes.head.fields.length == 1)
    assert(parsedFile.classes.head.fields(0).name == "a")
    assert(parsedFile.classes.head.fields(0).tpe == "Int")
  }

  test("parses Scala 3 derives annotation") {
    val file = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/Scala3Derives.scala_test")
      .getPath
    val parsedFile = FileParser.fromPathToClassDef(file)
    assert(parsedFile.classes.head.name == "Derives3")
    assert(parsedFile.classes.head.fields.length == 1)
    assert(parsedFile.classes.head.fields(0).name == "x")
    val derivesAnnotation =
      parsedFile.classes.head.annotations.find(_.name == "derives")
    assert(derivesAnnotation.isDefined)
    assert(derivesAnnotation.get.args.contains("ReadWriter"))
  }

  test("parses implicit ReadWriter instances") {
    val file = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/implicit-serialization.scala_test")
      .getPath
    val parsedFile = FileParser.fromPathToClassDef(file)

    // Should find all classes
    assert(parsedFile.classes.length == 5)

    // AchOriginationJobArgs should have implicit ReadWriter
    val achClass =
      parsedFile.classes.find(_.name == "AchOriginationJobArgs").get
    assert(achClass.fields.length == 3)
    assert(achClass.fields(0).name == "cutoff")
    assert(achClass.fields(2).name == "companyName")
    assert(
      achClass.fields(2).default.isDefined,
      "companyName should have default value"
    )
    // Should have ReadWriter annotation from implicit instance
    val achReadWriter = achClass.annotations.find(a =>
      (a.name == "deriving" || a.name == "derives" || a.name == "implicit") &&
        a.args.contains("ReadWriter")
    )
    assert(
      achReadWriter.isDefined,
      "AchOriginationJobArgs should have ReadWriter from implicit instance"
    )

    // UserWithWriter should have implicit Writer
    val userClass = parsedFile.classes.find(_.name == "UserWithWriter").get
    assert(userClass.fields.length == 2)
    val userWriter = userClass.annotations.find(a =>
      (a.name == "deriving" || a.name == "derives" || a.name == "implicit") &&
        a.args.contains("Writer")
    )
    assert(
      userWriter.isDefined,
      "UserWithWriter should have Writer from implicit instance"
    )

    // ProductWithReader should have implicit Reader
    val productClass =
      parsedFile.classes.find(_.name == "ProductWithReader").get
    assert(productClass.fields.length == 2)
    val productReader = productClass.annotations.find(a =>
      (a.name == "deriving" || a.name == "derives" || a.name == "implicit") &&
        a.args.contains("Reader")
    )
    assert(
      productReader.isDefined,
      "ProductWithReader should have Reader from implicit instance"
    )

    // OrderWithReadWriter should have implicit ReadWriter
    val orderClass =
      parsedFile.classes.find(_.name == "OrderWithReadWriter").get
    assert(orderClass.fields.length == 3)
    assert(orderClass.fields(2).name == "status")
    assert(
      orderClass.fields(2).default.isDefined,
      "status should have default value"
    )
    val orderReadWriter = orderClass.annotations.find(a =>
      (a.name == "deriving" || a.name == "derives" || a.name == "implicit") &&
        a.args.contains("ReadWriter")
    )
    assert(
      orderReadWriter.isDefined,
      "OrderWithReadWriter should have ReadWriter from implicit instance"
    )

    // NotSerializable should NOT have any serialization annotation
    val notSerializableClass =
      parsedFile.classes.find(_.name == "NotSerializable").get
    assert(notSerializableClass.fields.length == 2)
    val hasSerialization = notSerializableClass.annotations.exists(a =>
      (a.name == "deriving" || a.name == "derives" || a.name == "implicit") &&
        (a.args.contains("ReadWriter") || a.args.contains("Writer") || a.args
          .contains("Reader"))
    )
    assert(
      !hasSerialization,
      "NotSerializable should not have any serialization annotation"
    )
  }

  test(
    "parses implicit ReadWriter with qualified type (upickle.default.ReadWriter)"
  ) {
    val file = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("scala-3/implicit-qualified-type.scala_test")
      .getPath
    val parsedFile = FileParser.fromPathToClassDef(file)

    val testClass = parsedFile.classes.find(_.name == "TestClass").get
    assert(testClass.fields.length == 1)
    val readWriter = testClass.annotations.find(a =>
      a.name == "implicit" && a.args.contains("ReadWriter")
    )
    assert(
      readWriter.isDefined,
      "TestClass should have ReadWriter from implicit instance with qualified type"
    )
  }
}
