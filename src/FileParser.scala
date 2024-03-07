import scala.meta._
case class ScalaFile(
    imports: List[String],
    classes: List[ClassInfo],
    packageName: Option[String] = None
)
case class Annotation(name: String, args: List[String])
case class Field(name: String, tpe: String, default: Option[String])


case class ClassInfo(
    name: String,
    fields: List[Field],
    annotations: List[Annotation] = List.empty
) {
  override def toString =
    s"""|Class: $name
            |Fields: ${fields.mkString("\n")}
            |Annotations: ${annotations.mkString("\n")}
            |""".stripMargin
}


object FileParser {
  def praseTreeImport(tree: Tree): List[String] = {
    tree.children.collect {
      case i: Importer => List(i.toString)
      case x => praseTreeImport(x)
    }
  }.flatten

  def parseTreeClasses(classes: Tree) : List[Defn.Class] = {
    classes.children.collect {
      case c: Defn.Class => List(c)
      case x => parseTreeClasses(x)
    }.flatten
  }

  def addSuffixToDuplicates(list: List[ClassInfo]): List[ClassInfo] = {
    var countMap = collection.mutable.Map[String, Int]().withDefaultValue(0)
    list.map { c =>
      val count = countMap(c.name)
      countMap(c.name) += 1
      if (count > 0) c.copy(name = s"${c.name}$count") else c
    }
  }

  def parse(content: String): ScalaFile = {
    val input = Input.String(content)
    val exampleTree: Source = input.parse[Source].get
    
    val tree = 
      parseTreeClasses(exampleTree)
      .map(c =>
        (ClassInfo(
          c.name.value,
          c.ctor.paramss.flatten.map(p =>
            Field(
              p.name.value,
              p.decltpe.get.toString,
              p.default.map(_.toString)
            )
          ),
          c.mods
            .flatMap(_.children)
            .collect { 
              case Init(tpe, name, args) =>
                Annotation(tpe.toString, args.flatten.map(_.toString))
            }
        ))
      )
    ScalaFile(
      praseTreeImport(exampleTree),
      addSuffixToDuplicates(tree),
      exampleTree.children.collectFirst { case p: Pkg => p.ref.toString }
    )
  }
  def fromPathToClassDef(filePath: String): ScalaFile = {
    val path = java.nio.file.Paths.get(filePath)
    val bytes = java.nio.file.Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    parse(text)
  }
}
