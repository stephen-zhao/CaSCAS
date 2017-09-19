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

case class ApplyExpr(
  op:           Object,
  actualParams: Vector[Object]
) extends Expr
  with TypeIdentifier {

  def eval(ctx: Context): Evaluation = {

    // Evaluate the operator first, and extract the evaluated object (evaldOp)
    // and any reassignment changes to the context
    val evaldOpRes = this.op.eval(ctx)
    val evaldOp = evaldOpRes.evaldObj
    val evaldOpCtxDeltaOR = evaldOpRes.ctxDelta.onlyReassignments()

    // Assess the structure of the evaluated operator
    evaldOp match {
      // 1. it's a BuiltInExpr:
      case builtIn @ BuiltInExpr(formalParams, onApply, _, _) => {
        // Substitute in the actual parameters by assigning them to the formal
        // parameters in context to obtain a context mutation set (as well a
        // vector of leftover formal parameters, for the case of a partial
        // function application)
        val (subCtxDeltaA, leftOverParams) = this.subInRec(formalParams, actualParams)
        // 1.1. A full function application:
        if (leftOverParams.isEmpty) {
          // Process the parameters to obtain an explicit mapping from parameter
          // name to the value that is properly type-checked and a new context
          // with all changes from evaluating this Expr + processing the
          // parameters consolidated together.
          val (processedParams, processParamsCtxDeltaOR) = builtIn.processParams(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          )
          // Result is determined by the implementation of onApply
          // A promise is made that onApply will NEVER make persistent
          // alterations to the context, and so only reassignments from the
          // operator evaluation + parameter processing need to be back-
          // propagated.
          Evaluation(
            onApply(processedParams, ctx :+ (
              evaldOpCtxDeltaOR ++
              subCtxDeltaA ++
              processParamsCtxDeltaOR
            )),
            evaldOpCtxDeltaOR ++ processParamsCtxDeltaOR
          )
        }
        // 1.2. A partial function application:
        else {
          // is not allowed for BuiltInExprs
          throw new Exception("cannot partially apply built-in operator") //TODO
        }
      }
      // 2. it's an OperatorExpr with a LIRO body definition.
      case OperatorExpr(formalParams, body) => {
        // Substitute in the actual parameters by assigning them to the formal
        // parameters in context to obtain a context mutation set (as well a
        // vector of leftover formal parameters, for the case of a partial
        // function application)
        val (subCtxDeltaA, leftOverParams) = this.subInRec(formalParams, actualParams)
        // 2.1. A full function application:
        if (leftOverParams.isEmpty) {
          // Evaluate the body of the OperatorExpr with the modified context
          // (to include the reassignments from evaluating the operator and
          // the assignments from substituting in the parameters).
          val evaldBodyRes = body.eval(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          ).keepOnlyReassignments()
          // Return the evaluated body, and back-propagate all reassignments
          // made thus far.
          Evaluation(
            evaldBodyRes.evaldObj,
            evaldOpCtxDeltaOR ++ evaldBodyRes.ctxDelta
          )
        }
        // 2.2. A partial function application:
        else {
          // Evaluate the body of the OperatorExpr with the modified context
          // which includes the reassignments from evaluating the operator
          // and the assignments from partial substitution of the parameters.
          val evaldBodyRes = body.eval(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          ).keepOnlyReassignments()
          // Return the evaluated body wrapped in an OperatorExpr with the
          // leftover parameters to indicate work still needs to be done to
          // complete the function application. Back-propagate all
          // reassignments made thus far.
          Evaluation(
            OperatorExpr(leftOverParams, evaldBodyRes.evaldObj),
            evaldOpCtxDeltaOR ++ evaldBodyRes.ctxDelta
          )
        }
      }
      // 3. the structure cannot be used explicitly in a function application.
      case other => {
        // Return the same ApplyExpr except with the operator evaluated.
        // (parameters remain unevaluated) //TODO
        // Back propagate the reassignments from the operator evaluation.
        Evaluation(
          ApplyExpr(evaldOp, this.actualParams),
          evaldOpCtxDeltaOR
        )
      }
    }
  }

  @tailrec
  private def subInRec(
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object],
    ctxDelta:     ContextMutationSet = ContextMutationSet.empty
  ): (ContextMutationSet, Vector[FormalParameter]) = {
    if (formalParams.nonEmpty && actualParams.nonEmpty) {
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
        this.tryCheckAllRec(ctx, args, this.actualParams) match {
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
        this.tryCheckAllRec(ctx, args, this.actualParams) match {
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
    ctx:          Context,
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object]
  ): (Boolean, Vector[FormalParameter]) = {
    if (formalParams.nonEmpty && actualParams.nonEmpty) {
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