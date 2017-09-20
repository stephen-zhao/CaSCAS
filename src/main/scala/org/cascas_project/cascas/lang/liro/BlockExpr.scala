//=============================================================================
// lang/liro/BlockExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier
import scala.annotation.tailrec

//=============================================================================

case class BlockExpr(sequence: Vector[Object]) extends Expr {

  def eval(ctx: Context): Evaluation = {
    this.evalRecWithAllCtxDelta(this.sequence.toList, ctx).keepOnlyReassignments()
  }

  @tailrec
  private def evalRecWithAllCtxDelta(
    seqToProcess: List[Object],
    ctx:          Context,
    ctxDelta:     ContextMutationSet = ContextMutationSet.empty,
    seqAccum:     List[Object] = List()
  ): Evaluation = {
    seqToProcess.headOption match {
      case Some(obj) => {
        obj.eval(ctx) match {
          case Evaluation(evaldObj, evaldObjCtxDelta) => {
            this.evalRecWithAllCtxDelta(
              seqToProcess.tail,
              ctx :+ evaldObjCtxDelta,
              ctxDelta ++ evaldObjCtxDelta,
              evaldObj :: seqAccum
            )
          }
        }
      }
      case None => {
        Evaluation(seqAccum.head, ctxDelta)
      }
    }
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    this.evalRecWithAllCtxDelta(this.sequence.dropRight(1).toList, ctx) match {
      case Evaluation(_, evaldObjCtxDelta) => {
        this.sequence.last.checkType(ctx :+ evaldObjCtxDelta, tpe)
      }
    }
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    this.evalRecWithAllCtxDelta(this.sequence.dropRight(1).toList, ctx) match {
      case Evaluation(_, evaldObjCtxDelta) => {
        this.sequence.last.inferType(ctx :+ evaldObjCtxDelta)
      }
    }
  }

}
