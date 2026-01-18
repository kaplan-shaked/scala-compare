import upickle.default._
import ujson.Value

object BreakingChangeDetector {
  case class CompareSummary(
      className: String,
      removedFields: List[String],
      addedFieldsWithoutDefaultValues: List[String],
      changedDerivingAnnotation: Boolean,
      fieldsWithDefaultValuesThatWasRemoved: List[String] = List.empty,
      fieldsThatDefaultValueWasAddedToThem: List[String] = List.empty
  ) {
    override def toString: String =
      s"className: ${className}, Removed fields: $removedFields, Added fields without default value: $addedFieldsWithoutDefaultValues, Changed deriving annotation: $changedDerivingAnnotation, Fields with default values that was removed: $fieldsWithDefaultValuesThatWasRemoved"
    def isBreakingChange: Boolean =
      removedFields.nonEmpty || addedFieldsWithoutDefaultValues.nonEmpty || changedDerivingAnnotation || fieldsWithDefaultValuesThatWasRemoved.nonEmpty || fieldsThatDefaultValueWasAddedToThem.nonEmpty

  }

  object CompareSummary {
    implicit val rw: ReadWriter[CompareSummary] = macroRW
  }
  val serializableClasses =
    List("Reader", "Writer", "ResourceReader", "ResourceWriter", "ReadWriter")
  /*
    Detect braking change when a class removed existing field or added new field without default value.
   */
  def detectBreakingChange(
      oldFile: ScalaFile,
      newFile: ScalaFile
  ): List[CompareSummary] =
    newFile.classes
      .filter(checkIfClassHasSerialization(_, serializableClasses))
      .map(newClass =>
        oldFile.classes
          .find(_.name == newClass.name)
          .map(oldClass =>
            CompareSummary(
              oldClass.name,
              listOfRemovedFields(oldClass, newClass),
              listOfAddedFieldsWithoutDefaultValue(oldClass, newClass),
              checkIfSerializationWasChanged(oldClass, newClass),
              listOfFieldsThatDefaultValueWasRemoved(oldClass, newClass),
              listOfFieldsThatDefaultValueWasAdded(oldClass, newClass)
            )
          )
      )
      .flatten

  private def listOfRemovedFields(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): List[String] =
    oldClass.fields
      .filterNot(oldField => newClass.fields.exists(_.name == oldField.name))
      .map(_.name)

  private def listOfAddedFieldsWithoutDefaultValue(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): List[String] =
    newClass.fields
      .filterNot(newField => oldClass.fields.exists(_.name == newField.name))
      .filter(_.default.isEmpty)
      .map(_.name)

  /**
   * Check if class has serialization via @deriving or derives (old way)
   */
  private def checkIfClassHasDerivingAnnotation(
      classInfo: ClassInfo,
      initArgs: List[String]
  ): Boolean =
    classInfo.annotations.exists(x =>
      (x.name == "deriving" || x.name == "derives") && x.args.exists(initArgs.contains)
    )

  /**
   * Check if class has serialization via implicit val instances (new way)
   */
  private def checkIfClassHasImplicitSerialization(
      classInfo: ClassInfo,
      serializationTypes: List[String]
  ): Boolean =
    classInfo.annotations.exists(x =>
      x.name == "implicit" && x.args.exists(serializationTypes.contains)
    )

  /**
   * Check if class has serialization via either old or new way
   */
  private def checkIfClassHasSerialization(
      classInfo: ClassInfo,
      serializationTypes: List[String]
  ): Boolean =
    checkIfClassHasDerivingAnnotation(classInfo, serializationTypes) ||
      checkIfClassHasImplicitSerialization(classInfo, serializationTypes)
  private def listOfFieldsThatDefaultValueWasAdded(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): List[String] = newClass.fields
    .filter(newField =>
      oldClass.fields.exists(oldField =>
        oldField.name == newField.name && oldField.default.isEmpty && newField.default.isDefined
      )
    )
    .map(_.name)

  private def listOfFieldsThatDefaultValueWasRemoved(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): List[String] = oldClass.fields
    .filter(oldField =>
      newClass.fields.exists(newField =>
        oldField.name == newField.name && oldField.default.isDefined && newField.default.isEmpty
      )
    )
    .map(_.name)

  /**
   * Extract serialization types from annotations (old way: deriving/derives)
   */
  private def extractDerivingSerializationTypes(classInfo: ClassInfo): Set[String] = {
    classInfo.annotations
      .filter(x => x.name == "deriving" || x.name == "derives")
      .flatMap(_.args)
      .filter(serializableClasses.contains)
      .toSet
  }

  /**
   * Extract serialization types from implicit annotations (new way)
   */
  private def extractImplicitSerializationTypes(classInfo: ClassInfo): Set[String] = {
    classInfo.annotations
      .filter(_.name == "implicit")
      .flatMap(_.args)
      .filter(serializableClasses.contains)
      .toSet
  }

  /**
   * Get all serialization types (both old and new ways)
   */
  private def getAllSerializationTypes(classInfo: ClassInfo): Set[String] = {
    extractDerivingSerializationTypes(classInfo) ++ extractImplicitSerializationTypes(classInfo)
  }

  /**
   * Check if serialization was changed (handles both old and new ways, and migration between them)
   */
  private def checkIfSerializationWasChanged(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): Boolean = {
    val oldSerializationTypes = getAllSerializationTypes(oldClass)
    val newSerializationTypes = getAllSerializationTypes(newClass)

    // If old class had serialization, check if it changed
    if (oldSerializationTypes.nonEmpty) {
      // Serialization types changed (e.g., ReadWriter -> Writer, or deriving -> implicit)
      oldSerializationTypes != newSerializationTypes
    } else {
      // Old class didn't have serialization, so this is not a breaking change
      false
    }
  }
}
