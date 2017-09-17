//=============================================================================
// lang/liro/BuiltInExpr.scala : CaSCAS Project
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

case class BuiltInExpr(
  args: Vector[FormalParameter],
  onApply: (Context) => Evaluation,
  ret: TypeIdentifier,
  maybeOnEval: Option[(Context) => Evaluation]
) extends Expr {

  def eval(ctx: Context): Evaluation = this.maybeOnEval match {
    case Some(onEval) => onEval(ctx)
    case None => Evaluation(this, ContextMutationSet.empty)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe match {
      case OperatorType(args, ret) => {
        if (this.args.length == args.length) {
          if ((this.args zip args).forall{ case (a, b) => a == b}) {
            this.ret == ret
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
    Some(OperatorType(this.args, this.ret))
  }

}