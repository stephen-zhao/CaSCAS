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
  name:          String,
  formalParams:  Vector[FormalParameter],
  onApply:       (Map[String, Object], Context) => Object,
  ret:           TypeIdentifier,
  maybeOnEval:   Option[Context => Evaluation],
  maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String]
) extends Expr {

  def processParams(
    ctx: Context
  ): (Map[String, Object], ContextMutationSet) = {
    processParamsRec(formalParams, ctx)
  }

  @tailrec
  private def processParamsRec(
    fp:       Vector[FormalParameter],
    ctx:      Context,
    ctxDelta: ContextMutationSet = ContextMutationSet.empty,
    acc:      Map[String, Object] = Map()
  ): (Map[String, Object], ContextMutationSet) = {
    if (fp.isEmpty) {
      (acc, ctxDelta)
    }
    else {
      ctx.get(fp.head.id) match {
        case Some(TypedObject(tpe, value)) if tpe == fp.head.tpe => {
          value.eval(ctx).keepOnlyReassignments() match {
            case Evaluation(evaldValue, evaldCtxDeltaOR) => {
              processParamsRec(
                fp.tail,
                ctx :+ evaldCtxDeltaOR,
                ctxDelta ++ evaldCtxDeltaOR,
                acc + (fp.head.id.name -> evaldValue)
              )
            }
          }
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
    // 1. There is an explicitly defined behaviour for what to
    //    do upon eval, so call that.
    case Some(onEval) => onEval(ctx)
    // 2. There is no explicitly defined behaviour for what to
    //    do upon eval, so just return this as is.
    case None => Evaluation(this, ContextMutationSet.empty)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe match {
      case OperatorType(args, ret) => {
        if (this.formalParams.length == args.length) {
          if ((this.formalParams zip args).forall{ case (a, b) => a == b}) {
            this.ret == ret
          }
          else {
            throw new Exception("formalParams not same type") //TODO
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
    Some(OperatorType(this.formalParams, this.ret))
  }

  def toRepr(indentLevel: Int): String = {
    this.name
  }

}