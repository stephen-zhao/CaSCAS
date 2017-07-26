//=============================================================================
// lang/EvaluatedType.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

import org.cascas_project.cascas.shared.Enumerated

sealed abstract class EvaluatedType extends Obj with Enumerated.Value[EvaluatedType] {
  def enumType = EvaluatedType
}
final object EvaluatedType extends Enumerated.Type[EvaluatedType] {
  val enum = Vector(Bool, Number, Fraction, Float, Collection, List, Set, Operator)
}
final case object Bool       extends EvaluatedType {}
final case object Number     extends EvaluatedType {}
final case object Fraction   extends Number {}
final case object Float      extends Number {}
final case object Collection extends EvaluatedType {}
final case object List       extends Collection {}
final case object Set        extends Collection {}
final case object Operator   extends EvaluatedType {}
