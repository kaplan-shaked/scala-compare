object BreakingChangeDetector {
  case class CompareSummary(
      className: String,
      removedFields: List[String],
      addedFieldsWithoutDefaultValues: List[String],
      changedDerivingAnnotation: Boolean
  ) {
    override def toString: String =
      s"className: ${className}, Removed fields: $removedFields, Added fields without default value: $addedFieldsWithoutDefaultValues, Changed deriving annotation: $changedDerivingAnnotation"
    def isBreakingChange: Boolean =
      removedFields.nonEmpty || addedFieldsWithoutDefaultValues.nonEmpty || changedDerivingAnnotation
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
              checkIfDerivingAnnotationWasChanged(oldClass, newClass)
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
      x.name == "deriving" && x.args.find(initArgs.contains).isDefined
    )

  private def checkIfDerivingAnnotationWasChanged(
      oldClass: ClassInfo,
      newClass: ClassInfo
  ): Boolean =
    oldClass.annotations
      .filter(_.name == "deriving")
      .forall(oldAnnotation =>
        newClass.annotations
          .find(_.name == "deriving")
          .exists(newAnnotation =>
            newAnnotation.args.contains(oldAnnotation.args)
          )
      )

}
