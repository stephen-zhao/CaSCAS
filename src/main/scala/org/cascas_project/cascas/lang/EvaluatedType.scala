//=============================================================================
// lang/EvaluatedType.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

import org.cascas_project.cascas.shared.Enumerated

sealed abstract class EvaluatedTypeEnum extends Enumerated.Value[EvaluatedTypeEnum] {
  def enumType = EvaluatedTypeEnum
}
final object EvaluatedTypeEnum extends Enumerated.Type[EvaluatedTypeEnum] {
  val enum = Vector(BoolType, NumberType, IntegerType, FractionType, FloatType, 
                    CollectionType, ListType, SetType, OperatorType)
}
final case object BoolType       extends EvaluatedTypeEnum {}
final case object NumberType     extends EvaluatedTypeEnum {}
final case object IntegerType    extends EvaluatedTypeEnum {}
final case object FractionType   extends EvaluatedTypeEnum {}
final case object FloatType      extends EvaluatedTypeEnum {}
final case object CollectionType extends EvaluatedTypeEnum {}
final case object ListType       extends EvaluatedTypeEnum {}
final case object SetType        extends EvaluatedTypeEnum {}
final case object OperatorType   extends EvaluatedTypeEnum {}
