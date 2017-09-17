//=============================================================================
// lang/builtin/PlusOperator.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang._
import org.cascas_project.cascas.lang.liro.{ApplyExpr, Identifier, Object, RationalNumber}

//=============================================================================

object PlusOperator extends BuiltInDefinition {

  def onApply(ctx: Context): Evaluation = {

    // Get typed parameter "summands" from context
    // and assess its type
    ctx.get(Identifier("summands")) match {

      // 1. case SUCCESS, is typed as "List(Number)", then
      case Some(TypedObject(Identifier("List(Number)"), list)) => {

        // try to evaluate the parameter and assess structure
        list.eval(ctx).keepOnlyReassignments match {
          
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

      // 2. case FAIL, is not typed as "List(Number)", then
      case Some(TypedObject(other, _)) => {
        // report type mismatch error
        throw new Exception("what the fuck no1") //TODO
      }

      // 3. case FAIL, is not assigned, then
      case Some(other) => {
        // report unassigned error
        throw new Exception("what the fuck no3") //TODO
      }
      
      // 4. case FAIL, is not defined, then
      case None => {
        // report undefined error
        throw new Exception("what the fuck no2") //TODO
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
