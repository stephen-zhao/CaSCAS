//=============================================================================
// lang/liro/ApplyExpr.scala : CaSCAS Project
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