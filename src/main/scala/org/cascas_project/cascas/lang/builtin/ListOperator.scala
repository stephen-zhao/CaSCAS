//=============================================================================
// lang/builtin/ListOperator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.OperatorType
import org.cascas_project.cascas.lang.liro.Identifier

//=============================================================================

object ListOperator extends BuiltInDefinition with Unappliable {

  def tpe = OperatorType(Identifier("T"), Identifier("Type"))(Identifier("Type"))

  def ident = Identifier("List")

  def formalParams = Vector(FormalParameter(Identifier("T"), Identifier("Type")))

  def returnTpe = Identifier("Type")

}
