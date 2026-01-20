import scala.meta.*
import scala.meta.Dialect

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
      case x           => praseTreeImport(x)
    }
  }.flatten

  def parseTreeClasses(classes: Tree): List[Defn.Class] = {
    classes.children.collect {
      case c: Defn.Class => List(c)
      case x             => parseTreeClasses(x)
    }.flatten
  }

  /** Parse implicit val declarations from the AST. Returns a map from class
    * name to serialization type (ReadWriter, Writer, Reader, etc.)
    */
  private def parseImplicitSerializationInstances(
      tree: Tree
  ): Map[String, String] = {
    def extractFromType(tpe: Type): Option[(String, String)] = tpe match {
      case Type.Apply.After_4_6_0(Type.Name(serializationType), argClause) =>
        argClause.values.headOption.collect { case Type.Name(className) =>
          (className, serializationType)
        }
      case Type.Apply.After_4_6_0(
            Type.Select(_, Type.Name(serializationType)),
            argClause
          ) =>
        argClause.values.headOption.collect { case Type.Name(className) =>
          (className, serializationType)
        }
      case _ => None
    }

    def collectImplicitVals(tree: Tree): List[(String, String)] = {
      tree.children.flatMap {
        case v: Defn.Val if v.mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          v.decltpe.flatMap(extractFromType).toList
        case x => collectImplicitVals(x)
      }
    }

    collectImplicitVals(tree).toMap
  }

  def addSuffixToDuplicates(list: List[ClassInfo]): List[ClassInfo] = {
    var countMap = collection.mutable.Map[String, Int]().withDefaultValue(0)
    list.map { c =>
      val count = countMap(c.name)
      countMap(c.name) += 1
      if (count > 0) c.copy(name = s"${c.name}$count") else c
    }
  }

  /** Extract annotations from @deriving or derives syntax (old way)
    */
  private def extractDerivingAnnotations(
      classDef: Defn.Class
  ): List[Annotation] = {
    val derivingFromAnnotation =
      classDef.mods
        .flatMap(_.children)
        .collect { case Init.After_4_6_0(tpe, _, argClauses) =>
          Annotation(
            tpe.toString,
            argClauses.flatMap(_.values).map(_.toString).toList
          )
        }
    val derivingFromDerives =
      classDef.templ.derives.map(d => Annotation("derives", List(d.toString)))
    derivingFromAnnotation ++ derivingFromDerives
  }

  /** Extract annotations from implicit val instances (new way)
    */
  private def extractImplicitSerializationAnnotations(
      className: String,
      implicitInstances: Map[String, String]
  ): List[Annotation] = {
    implicitInstances
      .get(className)
      .map(serializationType => Annotation("implicit", List(serializationType)))
      .toList
  }

  def parse(
      content: String,
      dialect: Dialect = dialects.Scala3
  ): ScalaFile = {
    val input = Input.String(content)
    val exampleTree: Source = dialect(input).parse[Source].get

    // Parse implicit serialization instances (new way)
    val implicitInstances = parseImplicitSerializationInstances(exampleTree)

    // Parse classes with their annotations (old way)
    val tree =
      parseTreeClasses(exampleTree)
        .map(c => {
          val derivingAnnotations = extractDerivingAnnotations(c)
          val implicitAnnotations = extractImplicitSerializationAnnotations(
            c.name.value,
            implicitInstances
          )
          ClassInfo(
            c.name.value,
            c.ctor.paramClauses.headOption
              .map(_.values)
              .getOrElse(Nil)
              .map(p =>
                Field(
                  p.name.value,
                  p.decltpe.get.toString,
                  p.default.map(_.toString)
                )
              ),
            derivingAnnotations ++ implicitAnnotations
          )
        })
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
    parse(text, dialects.Scala3)
  }
}
