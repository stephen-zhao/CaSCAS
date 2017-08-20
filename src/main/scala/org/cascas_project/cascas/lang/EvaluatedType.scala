//=============================================================================
// lang/EvaluatedType.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

import org.cascas_project.cascas.shared.Enumerated

//=============================================================================
// An enumeration used to represent the various kinds of evaluated types for
// use before the AST is converted to a CaSCAS internal representation
//

object EvaluatedType {
  //sealed abstract class EvaluatedTypeEnum extends Enumerated.Value[EvaluatedTypeEnum] {
  //  def enumType = EvaluatedTypeEnum
  //}
  //final object EvaluatedTypeEnum extends Enumerated.Type[EvaluatedTypeEnum] {
  //  val enum = Vector(BoolType, NumberType, IntegerType, FractionType, FloatType, 
  //                    CollectionType, ListType, SetType, OperatorType)
  //}
  sealed abstract class EvaluatedTypeLabel {}
  final case object UnTyped         extends EvaluatedTypeLabel {}
  final case object BoolType        extends EvaluatedTypeLabel {}
  final case object NumberType      extends EvaluatedTypeLabel {}
  final case object CollectionType  extends EvaluatedTypeLabel {}
  final case object ListType        extends EvaluatedTypeLabel {}
  final case object SetType         extends EvaluatedTypeLabel {}
  final case object OperatorType    extends EvaluatedTypeLabel {}
  final case class  OperatorType(
    params: Vector[Option[EvaluatedTypeLabel]],
    result: Option[EvaluatedTypeLabel]
  ) extends EvaluatedTypeLabel {}
}

//=============================================================================
// The EvaluatedType class will have all possible evaluated types extended from
//
abstract class EvaluatedType extends Obj {
  //TODO
}
