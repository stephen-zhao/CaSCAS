//=============================================================================
// lang/builtin/PlusOperator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang._
import org.cascas_project.cascas.lang.liro.{ApplyExpr, Identifier, Object, RationalNumber}

//=============================================================================

object PlusOperator extends BuiltInDefinition {

  def onApply(v : Vector[Object], ctx: Context): Evaluation = {
   
        // try to evaluate the parameter and assess structure
        v(0).eval(ctx).keepOnlyReassignments match {
          
          // 1.1. case SUCCESS, is a ListExpr, then
          // do the evaluation
          case Evaluation(ListExpr(summands), ctxDelta) => {
            val res = summands /: (Accum(ListExpr(), RationalNumber.zero))(addIfPossible _)
            Evaluation(res.nonconstTerms :+ res.constTerm, ctxDelta)
          }

          // 1.2. case FAIL, is not a ListExpr, then
          // cannot do evaluation, return as is
          case Evaluation(other, ctxDelta) => {
            Evaluation(ApplyExpr(PlusOperator.ident, Vector(other)), ctxDelta)
          }
        }
    }

  private def addIfPossible(accum: Accum, currentTerm: Object): Accum = {
    currentTerm match {
      case const: RationalNumber => Accum(accum.nonconstTerms, accum.constTerm + const)
      case term => Accum(accum.nonconstTerms :+ term, accum.constTerm)
    }
  }

  def tpe = OperatorType(Identifier("summands"), Identifier("List(Number)"))(Identifier("Number"))

  def ident = Identifier("+")

  def formalParams = Vector(FormalParameter(Identifier("summands"), Identifier("List(Number)")))

  def returnTpe = Identifier("Number")

  case class Accum(nonconstTerms: ListExpr, constTerm: RationalNumber)

}
