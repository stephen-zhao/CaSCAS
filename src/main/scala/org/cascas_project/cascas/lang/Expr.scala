package org.cascas_project.cascas.lang

import scala.annotation.tailrec




case class Evaluation(evaldObj: Object, ctxDelta: ContextMutationSet) {
  def keepOnlyReassignments(): Evaluation = {
    Evaluation(evaldObj, ctxDelta.onlyReassignments())
  }
}




trait Expr extends Object {}




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




case class ApplyExpr(op: Object, params: Vector[Object]) extends Expr {

  def eval(ctx: Context): Evaluation = {
    
    val evaldOpRes = this.op.eval(ctx)
    val evaldOp = evaldOpRes.evaldObj
    val evaldOpCtxDelta = evaldOpRes.ctxDelta.onlyReassignments

    evaldOp match {
      case BuiltInExpr(args, onApply, _, _) => {
        val (subCtxDelta, leftOverParams) = this.subInRec(args, params)
        if (leftOverParams.isEmpty) {
          onApply(
            ctx.consolidatedWith(evaldOpCtxDelta ++ subCtxDelta)
          ).keepOnlyReassignments
        }
        else {
          throw new Exception("cannot partially apply built-in operator") //TODO
        }
      }
      case OperatorExpr(args, body) => {
        val (subCtxDelta, leftOverParams) = this.subInRec(args, params)
        if (leftOverParams.isEmpty) {
          body.eval(
            ctx.consolidatedWith(evaldOpCtxDelta ++ subCtxDelta)
          ).keepOnlyReassignments
        }
        else {
          val evaldBodyRes = body.eval(
            ctx.consolidatedWith(evaldOpCtxDelta ++ subCtxDelta)
          ).keepOnlyReassignments
          Evaluation(
            OperatorExpr(leftOverParams, evaldBodyRes.evaldObj),
            evaldBodyRes.ctxDelta.onlyReassignments
          )
        }
      }
      case other => {
        throw new Exception("not operator expr") //TODO
      }
    }
  }

  @tailrec
  private def subInRec(
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object],
    ctxDelta: ContextMutationSet = ContextMutationSet.empty
  ): (ContextMutationSet, Vector[FormalParameter]) = {
    if (!formalParams.isEmpty && !actualParams.isEmpty) {
      ctxDelta.assign(
        formalParams.head.id,
        TypedObject(formalParams.head.tpe, actualParams.head)
      )
      this.subInRec(formalParams.tail, actualParams.tail, ctxDelta)
    }
    else if (actualParams.isEmpty) {
      (ctxDelta, formalParams)
    }
    else {
      throw new Exception("more actual params than formal params") //TODO
    }
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    // Infer type of the operator
    this.op.inferType(ctx) match {
      case Some(OperatorType(args, ret)) => {
        // Check as many formal parameters as there are actual parameters
        // for type match. If there are any formal parameters leftover,
        // then the type to check against tpe will be the OperatorType on
        // the remaining formal parameters. If there are none leftover, then
        // then type to check against tpe will be the return type of the
        // operator. Otherwise, it does not type check.
        this.tryCheckAllRec(ctx, args, this.params) match {
          case (false, _) => false
          case (true, Vector()) => {
            tpe == ret
          }
          case (true, leftOverParams) => {
            tpe == OperatorType(leftOverParams, ret)
          }
        }
      }
      case Some(other) => false
      case None => false
    }
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    // First infer the type of the operator
    this.op.inferType(ctx) match {
      case Some(OperatorType(args, ret)) => {
        // If it is an operator, check the types of its parameters
        // until no actual parameters are left. If some formal parameters
        // are left-over, the type of this apply node must be an OperatorType
        // on the remaining formal parameters + return type. Else if no formal
        // parameters are left, this apply node must be the return type of
        // the operator. Otherwise, type inference fails.
        this.tryCheckAllRec(ctx, args, this.params) match {
          case (false, _) => None
          case (true, Vector()) => {
            Some(ret)
          }
          case (true, leftOverParams) => {
            Some(OperatorType(leftOverParams, ret))
          }
        }
      }
      case Some(other) => {
        // If it is not an operator, then type inference fails
        None
      }
      case None => {
        // If the operator failed to type-infer, then type inference fails
        None
      }
    }
  }

  @tailrec
  private def tryCheckAllRec(
    ctx: Context,
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object]
  ): (Boolean, Vector[FormalParameter]) = {
    if (!formalParams.isEmpty && !actualParams.isEmpty) {
      if (actualParams.head.checkType(ctx, formalParams.head.tpe)) {
        this.tryCheckAllRec(
          ctx.consolidatedWith(ContextMutationSet.empty.assign(
            formalParams.head.id,
            TypedObject(formalParams.head.tpe, actualParams.head)
          )),
          formalParams.tail,
          actualParams.tail
        )
      }
      else {
        (false, formalParams)
      }
    }
    else if (actualParams.isEmpty) {
      (true, formalParams)
    }
    else {
      (false, formalParams)
    }
  }

}




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

}




//TODO
//case class TestAndBody(test: Object, body: Object)
//
//case class IfExpr(ifdo: Vector[TestAndBody], elsedo: Object) extends Expr {
//  
//}

