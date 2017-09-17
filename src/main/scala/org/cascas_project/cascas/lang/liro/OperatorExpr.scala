//=============================================================================
// lang/liro/OperatorExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.OperatorType
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

case class OperatorExpr(args: Vector[FormalParameter], body: Object) extends Expr {

  def eval(ctx: Context): Evaluation = {
    val evaldBody = this.body.eval(
      ctx.consolidatedWith(ContextMutationSet.empty.withIntroductions(this.args))
    ).keepOnlyReassignments
    Evaluation(OperatorExpr(this.args, evaldBody.evaldObj), evaldBody.ctxDelta)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe match {
      case OperatorType(args, ret) => {
        if (this.args.length == args.length) {
          if ((this.args zip args).forall{ case (a, b) => a == b }) {
            this.body.checkType(
              ctx.consolidatedWith(ContextMutationSet.empty.withIntroductions(this.args)),
              ret
            )
          }
          else {
            throw new Exception("args not same type") //TODO
          }
        }
        else {
          throw new Exception("not same length args") //TODO
        }
      }
      case other => {
        throw new Exception("not operator type") //TODO
      }
    }
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    None //TODO
  }

}


