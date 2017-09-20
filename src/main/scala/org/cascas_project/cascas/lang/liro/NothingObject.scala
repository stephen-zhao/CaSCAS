//=============================================================================
// lang/liro/AssignmentExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

case class NothingObject() extends Literal {
  def eval(ctx: Context): Evaluation = Evaluation(this, ContextMutationSet.empty)
  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = true
  def inferType(ctx: Context): Option[TypeIdentifier] = None
  def toRepr: String = "<<NOTHING>>" //TODO
}
