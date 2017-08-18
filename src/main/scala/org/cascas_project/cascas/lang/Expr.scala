package org.cascas_project.cascas.lang

import scala.util.annotation.tailrec

trait Expr extends Object {}




case class OperatorExpr(op: Object, params: Vector[Object]) extends Expr {

}




case class WhileExpr(predicate: Object, body: Object) extends Expr {
  
  def eval(ctx: Context): Evaluation = {

    @tailrec
    private def evalRec(ctx: Context, accum: Vector[Object]): (Vector[Object], Context) = {
      val (evaldPredicate, ctxDeltaPredicate) = this.predicate.eval(ctx)
      if (evaldPredicate.checkType(ctx, Identifier("Bool"))) {
          if (evaldPredicate.isTrue()) {
            val (evaldBody, ctxDeltaBody) = this.body.eval(ctx)
            evalRec(ctx.mutateWith(ctxDeltaBody), accum :+ evaldBody)
          }
          else {
            (accum, ctx)
          }
      }
      else {
        throw new Exception("bad type") //TODO
      }
    }

    val (evaldObjs, ctxNew) = evalRec(ctx, Vector[Object]())
    val ctxDelta = ctx.getDeltaTo(ctxNew)

    Evaluation(makeList(evaldObjs), ctxDelta)

  }

}




case class TestAndBody(test: Object, body: Object)

case class IfExpr(ifdo: Vector[TestAndBody], elsedo: Object) extends Expr {
  
}

case class Evaluation(evaldObj: Object, ctxDelta: ContextMutationSet)
