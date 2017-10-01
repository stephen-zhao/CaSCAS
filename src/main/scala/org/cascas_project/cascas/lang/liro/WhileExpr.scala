//=============================================================================
// lang/liro/WhileExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

case class WhileExpr(predicate: Object, body: Object) extends Expr {

  //  def eval(ctx: Context): Evaluation = {
  //
  //    @tailrec
  //    def evalRec(ctx: Context, accum: Vector[Object]): (Vector[Object], Context) = {
  //      val (evaldPredicate, ctxDeltaPredicate) = this.predicate.eval(ctx)
  //      if (evaldPredicate.checkType(ctx, Identifier("Bool"))) {
  //        if (evaldPredicate.isTrue()) {
  //          val (evaldBody, ctxDeltaBody) = this.body.eval(ctx)
  //          evalRec(ctx.mutateWith(ctxDeltaBody), accum :+ evaldBody)
  //        }
  //        else {
  //          (accum, ctx)
  //        }
  //      }
  //      else {
  //        throw new Exception("bad type") //TODO
  //      }
  //    }
  //
  //    val (evaldObjs, ctxNew) = evalRec(ctx, Vector[Object]())
  //    val ctxDelta = ctx.getDeltaTo(ctxNew)
  //
  //    Evaluation(makeList(evaldObjs), ctxDelta)
  //
  //  }

  def eval(ctx: Context): Evaluation = {
    Evaluation(this, ContextMutationSet.empty)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    //TODO
    true
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    //TODO
    None
  }

  def inferTheirTypes(
    ctx: Context,
    themToTheirMaybeTypes: Map[Identifier, Option[TypeIdentifier]]
  ): Map[Identifier, Option[TypeIdentifier]] = {
    themToTheirMaybeTypes //TODO: make this actually work
  }

  def toRepr(indentLevel: Int): String = {
    //TODO
    ""
  }

}