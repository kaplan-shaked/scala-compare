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
      .filter(checkIfClassHasDerivingAnnotation(_, serializableClasses))
      .map(newClass =>
        oldFile.classes
          .find(_.name == newClass.name)
          .map(oldClass =>
            CompareSummary(
              oldClass.name,
              listOfRemovedFields(oldClass, newClass),
              listOfAddedFieldsWithoutDefaultValue(oldClass, newClass),
              checkIfDerivingAnnotationWasChanged(oldClass, newClass),
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

  private def checkIfClassHasDerivingAnnotation(
      classInfo: ClassInfo,
      initArgs: List[String]
  ): Boolean =
    classInfo.annotations.exists(x =>
      (x.name == "deriving" || x.name == "derives") && x.args.exists(initArgs.contains)
    )
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

  private def checkIfDerivingAnnotationWasChanged(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): Boolean = {
    val isOldClassContainsSerializable = !(oldClass.annotations
      .filter(x => x.name == "deriving" || x.name == "derives")
      .flatMap(_.args)
      .filter(x => serializableClasses.contains(x))
      .length == 0)

    val oldClassDerivingAnnotations =
      oldClass.annotations.filter(x => x.name == "deriving" || x.name == "derives")
    val newClassDerivingAnnotations =
      newClass.annotations.filter(x => x.name == "deriving" || x.name == "derives")

    isOldClassContainsSerializable &&
    !oldClassDerivingAnnotations
      .forall(oldAnnotation =>
        newClassDerivingAnnotations.exists(newAnnotation => {
          oldAnnotation.args.toSet.subsetOf(newAnnotation.args.toSet)
        })
      )
  }
}
