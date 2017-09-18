//=============================================================================
// lang/builtin/MultiplyOperator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.OperatorType
import org.cascas_project.cascas.lang.liro.ApplyExpr
import org.cascas_project.cascas.lang.liro.Identifier
import org.cascas_project.cascas.lang.liro.Object
import org.cascas_project.cascas.lang.liro.RationalNumber

//=============================================================================

object MultiplyOperator extends BuiltInDefinition {
  //v is the processed vector of arguments, l is 
  def onApply(v : Vector[Object], ctx: Context): Evaluation = {
   
        // try to evaluate the parameter and assess structure
        v(0).eval(ctx).keepOnlyReassignments match {
          
          // 1.1. case SUCCESS, is a ListExpr, then
          // do the evaluation
          case Evaluation(ListExpr(multiplicands), ctxDelta) => {
            val res = multiplicands /: (Accum(ListExpr(), RationalNumber.zero))(multiplyIfPossible _)
            Evaluation(res.nonconstTerms :+ res.constTerm, ctxDelta)
          }

          // 1.2. case FAIL, is not a ListExpr, then
          // cannot do evaluation, return as is
          case Evaluation(other, ctxDelta) => {
            Evaluation(ApplyExpr(MultiplyOperator.ident, Vector(other)), ctxDelta)
          }
        }
  }

  private def multiplyIfPossible(accum: Accum, currentTerm: Object): Accum = {
    currentTerm match {
      case const: RationalNumber => Accum(accum.nonconstTerms, accum.constTerm * const)
      case term => Accum(accum.nonconstTerms :+ term, accum.constTerm)
    }
  }

  def tpe = OperatorType(Identifier("multiplicands"), Identifier("List(Number)"))(Identifier("Number"))

  def ident = Identifier("*")

  def formalParams = Vector(FormalParameter(Identifier("multiplicands"), Identifier("List(Number)")))

  def returnTpe = Identifier("Number")

  case class Accum(nonconstTerms: ListExpr, constTerm: RationalNumber)

}
