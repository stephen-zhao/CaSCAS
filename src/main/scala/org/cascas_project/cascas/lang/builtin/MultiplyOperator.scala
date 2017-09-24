//=============================================================================
// lang/builtin/MultiplyOperator.scala : CaSCAS Project
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

object MultiplyOperator extends BuiltInDefinition with AppearsAsBinaryInfixOp {

  def onApply(params : Map[String, Object], ctx: Context): Object = {

    // Assess the structure of the multiplicands parameter
    params("multiplicands") match {

      // 1. case SUCCESS, is a ListExpr, then
      // do the multiplication
      case ListExpr(multiplicands) => {
        val res = (multiplicands foldLeft Accum(ListExpr(), RationalNumber.zero))(multiplyIfPossible _)
        res.nonconstFactors :+ res.constFactor
      }

      // 2. case FAIL, is not a ListExpr, then
      // cannot do multiplication on a non-explicit list, return as is
      case other => {
        ApplyExpr(MultiplyOperator.ident, Vector(other))
      }
    }
  }

  private def multiplyIfPossible(accum: Accum, currentFactor: Object): Accum = {
    currentFactor match {
      case const: RationalNumber => Accum(accum.nonconstFactors, accum.constFactor * const)
      case term => Accum(accum.nonconstFactors :+ term, accum.constFactor) //TODO: logic to collect like factors
    }
  }

  def ident = Identifier("*")

  def formalParams = Vector(
    FormalParameter(Identifier("multiplicands"), ApplyExpr(Identifier("List"), Vector(Identifier("Number"))))
  )

  def returnTpe = Identifier("Number")

  case class Accum(nonconstFactors: ListExpr, constFactor: RationalNumber)

}
