//=============================================================================
// lang/Manyness.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.shared.Enumerated

//=============================================================================

sealed abstract class Manyness extends Enumerated.Value[Manyness] {
  def enumType = Manyness
}
object Manyness extends Enumerated.Type[Manyness] {
  val enum = Vector(One, Many)
}
case object One extends Manyness {}
case object Many extends Manyness {}