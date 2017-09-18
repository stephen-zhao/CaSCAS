//=============================================================================
// lang/builtin/AdditionOperator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.liro.ApplyExpr
import org.cascas_project.cascas.lang.liro.Identifier
import org.cascas_project.cascas.lang.liro.ListExpr
import org.cascas_project.cascas.lang.liro.Object
import org.cascas_project.cascas.lang.liro.RationalNumber

//=============================================================================

object AdditionOperator extends BuiltInDefinition {

  def onApply(params: Map[String, Object], ctx: Context): Object = {
   
    // Assess the structure of the addends parameter
    params("summands") match {

      // 1. case SUCCESS, is a ListExpr, then
      // do the addition
      case ListExpr(summands) => {
        val res = (summands foldLeft Accum(ListExpr(), RationalNumber.zero))(addIfPossible _)
        res.nonconstTerms :+ res.constTerm
      }

      // 2. case FAIL, is not a ListExpr, then
      // cannot do addition on a non-explicit list, return as is
      case other => {
        ApplyExpr(AdditionOperator.ident, Vector(other))
      }
    }
  }

  private def addIfPossible(accum: Accum, currentTerm: Object): Accum = {
    currentTerm match {
      case const: RationalNumber => Accum(accum.nonconstTerms, accum.constTerm + const)
      case term => Accum(accum.nonconstTerms :+ term, accum.constTerm) //TODO: logic to collect like terms
    }
  }

  def ident = Identifier("+")

  def formalParams = Vector(
    FormalParameter(Identifier("summands"), ApplyExpr(Identifier("List"), Vector(Identifier("Number"))))
  )

  def returnTpe = Identifier("Number")

  case class Accum(nonconstTerms: ListExpr, constTerm: RationalNumber)

}
