package org.cascas_project.cascas.lang.builtin

//import org.cascas_project.cascas.util.ImplicitConversions
import org.cascas_project.cascas.lang._

object PlusOperator extends BuiltInDefObj {

  def onApply(ctx: Context): Evaluation = {
    ctx.get(Identifier("summands")) match {
      case Some(TypedObject(Identifier("List"), list)) => {
        list.eval(ctx).keepOnlyReassignments match {
          case Evaluation(ListExpr(summands), ctxDelta) => {
            val res = summands /: (Accum(ListExpr(), RationalNumber.zero))(addIfPossible _)
            Evaluation(res.nonconstTerms :+ res.constTerm, ctxDelta)
          }
          case other => {
            throw new Exception("what the fuck no3") //TODO
          }
        }
      }
      case Some(other) => {
        throw new Exception("what the fuck no1") //TODO
      }
      case None => {
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

  def tpe = OperatorType(Identifier("summands"), Identifier("List"))(Identifier("Number"))

  def formalParams = Vector(FormalParameter(Identifier("summands"), Identifier("Number"), Many))

  def returnTpe = Identifier("Number")

  case class Accum(nonconstTerms: ListExpr, constTerm: RationalNumber)

}
