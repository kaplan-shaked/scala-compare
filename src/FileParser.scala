import scala.meta._
case class ScalaFile(imports: List[String], classes: List[ClassInfo], packageName: Option[String] = None) 
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
  def parse(content: String): ScalaFile = {
    val input = Input.String(content)
    val exampleTree: Source = input.parse[Source].get

    val tree = exampleTree.children
      .collect { case c: Defn.Class =>
        c
      }
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
            .map{
                case Init(tpe, name, args) => Annotation(tpe.toString, args.flatten.map(_.toString))
            }
        ))
      )
    ScalaFile(
      exampleTree.children.collect { case i: Importer => i.toString },
      tree,
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
