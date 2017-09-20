//=============================================================================
// lang/liro/AssignmentExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier
import org.cascas_project.cascas.lang.TypedObject

//=============================================================================

case class AssignmentExpr(
  identifier: Identifier,
  value:      Object
) extends Expr {

  def eval(ctx: Context): Evaluation = {
    // To evaluate an assignment, carry out the context modification, and back propagate it
    Evaluation(NothingObject(), ContextMutationSet.empty.assign(
      this.identifier,
      TypedObject(this.value.inferType(ctx).getOrElse(Identifier("T")), this.value) //TODO handle bad type inference
    ))
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = true //TODO

  def inferType(ctx: Context): Option[TypeIdentifier] = None //TODO

  def toRepr: String = {
    "let " + identifier.toRepr + " := " + value.toRepr
  }

}
