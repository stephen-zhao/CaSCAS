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
import org.cascas_project.cascas.lang.TypedObject
import scala.annotation.tailrec

//=============================================================================

case class BuiltInExpr(
  args:        Vector[FormalParameter],
  onApply:     (Vector[Object], Context) => Evaluation,
  ret:         TypeIdentifier,
  maybeOnEval: Option[(Context) => Evaluation]
) extends Expr {

  def processParams(ctx: Context): Vector[Object] = {
    var temp: Vector[Object] = Vector()
    processParamsRec(args, ctx, temp)
  }

  @tailrec
  private def processParamsRec(
    fp:  Vector[FormalParameter],
    ctx: Context,
    acc: Vector[Object]
  ): Vector[Object] = {
    if (fp.isEmpty) {
      acc
    }
    else {
      ctx.get(fp.head.id) match {
        case Some(TypedObject(tpe, value)) if tpe == fp.head.tpe => {
          processParamsRec(fp.tail, ctx, acc :+ value)
        }

        case Some(TypedObject(other, _)) => {
          // report type mismatch error
          throw new Exception("type mismatch error") //TODO
        }

        // 3. case FAIL, is not assigned, then
        case Some(other) => {
          // report unassigned error
          throw new Exception("is not assigned") //TODO
        }

        // 4. case FAIL, is not defined, then
        case None => {
          // report undefined error
          throw new Exception("is not defined") //TODO
        }
      }
    }
  }

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