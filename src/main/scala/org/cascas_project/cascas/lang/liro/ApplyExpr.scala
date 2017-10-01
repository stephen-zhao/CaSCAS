//=============================================================================
// lang/liro/ApplyExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.Logger
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
    this.evalLogVerbose(s"1. Evaluating operator $op ...")
    val evaldOpRes = this.op.eval(ctx)
    val evaldOp = evaldOpRes.evaldObj
    val evaldOpCtxDeltaOR = evaldOpRes.ctxDelta.onlyReassignments()
    this.evalLogVerbose(Vector(
      "2.",
      "Operator evaluated.",
      s"Evaluated operator: ${evaldOp.toRepr()}",
      s"Reassignments: ${evaldOpCtxDeltaOR.getReassignments}"
    ))

    // Assess the structure of the evaluated operator
    this.evalLogVerbose("3. Assessing structure of evaluated operator...")
    evaldOp match {
      // 1. it's a BuiltInExpr:
      case builtIn @ BuiltInExpr(_, formalParams, onApply, _, _, _) => {
        this.evalLogVerbose("4. Structural assessment complete. BuiltInExpr detected.")
        // Substitute in the actual parameters by assigning them to the formal
        // parameters in context to obtain a context mutation set (as well a
        // vector of leftover formal parameters, for the case of a partial
        // function application)
        this.evalLogVerbose("5. Substituting actual parameters by adding to context...")
        val (subCtxDeltaA, leftOverParams) = this.subInRec(formalParams, actualParams)
        this.evalLogVerbose("6. Actual parameters substitution complete.")
        this.evalLogVerbose("7. Assessing the degree of operator application...")
        // 1.1. A full function application:
        if (leftOverParams.isEmpty) {
          this.evalLogVerbose("8. Full operator application detected (no left-over formal parameters).")
          // Process the parameters to obtain an explicit mapping from parameter
          // name to the value that is properly type-checked and a new context
          // with all changes from evaluating this Expr + processing the
          // parameters consolidated together.
          this.evalLogVerbose("9. Processing parameters...")
          val (processedParams, processParamsCtxDeltaOR) = builtIn.processParams(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          )
          this.evalLogVerbose(Vector(
            "10.",
            "Parameters processing complete.",
            s"Processed parameters: $processedParams",
            s"Reassignments: ${processParamsCtxDeltaOR.getReassignments}"
          ))
          // Result is determined by the implementation of onApply
          // A promise is made that onApply will NEVER make persistent
          // alterations to the context, and so only reassignments from the
          // operator evaluation + parameter processing need to be back-
          // propagated.
          this.evalLogVerbose("11. Producing evaluation...")
          val res = Evaluation(
            onApply(
              processedParams, ctx :+ (
                evaldOpCtxDeltaOR ++
                  subCtxDeltaA ++
                  processParamsCtxDeltaOR
                )
            ),
            evaldOpCtxDeltaOR ++ processParamsCtxDeltaOR
          )
          this.evalLogVerbose(Vector(
            "12.",
            "Evaluation produced.",
            s"Result from built-in operator application: ${res.evaldObj}",
            s"Resultant context reassignments: ${res.ctxDelta.getReassignments}"
          ))
          res
        }
        // 1.2. A partial function application:
        else {
          // is not allowed for BuiltInExprs
          this.evalLogVerbose("8. Partial operator application detected (left-over formal parameters exist).")
          throw new Exception("cannot partially apply built-in operator") //TODO
        }
      }
      // 2. it's an OperatorExpr with a LIRO body definition.
      case OperatorExpr(formalParams, body) => {
        this.evalLogVerbose("4. Structural assessment complete. OperatorExpr detected.")
        // Substitute in the actual parameters by assigning them to the formal
        // parameters in context to obtain a context mutation set (as well a
        // vector of leftover formal parameters, for the case of a partial
        // function application)
        this.evalLogVerbose("5. Substituting actual parameters by adding to context...")
        val (subCtxDeltaA, leftOverParams) = this.subInRec(formalParams, actualParams)
        this.evalLogVerbose("6. Actual parameters substitution complete.")
        this.evalLogVerbose("7. Assessing the degree of operator application...")
        // 2.1. A full function application:
        if (leftOverParams.isEmpty) {
          this.evalLogVerbose("8. Full operator application detected (no left-over formal parameters).")
          // Evaluate the body of the OperatorExpr with the modified context
          // (to include the reassignments from evaluating the operator and
          // the assignments from substituting in the parameters).
          this.evalLogVerbose("9. Evaluating body of operator...")
          val evaldBodyRes = body.eval(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          ).keepOnlyReassignments()
          this.evalLogVerbose("10. Evaluation of body complete.")
          // Return the evaluated body, and back-propagate all reassignments
          // made thus far.
          this.evalLogVerbose("11. Producing evaluation...")
          val res = Evaluation(
            evaldBodyRes.evaldObj,
            evaldOpCtxDeltaOR ++ evaldBodyRes.ctxDelta
          )
          this.evalLogVerbose(Vector(
            "12.",
            "Evaluation produced.",
            s"Result from full operator application: ${res.evaldObj}",
            s"Resultant context reassignments: ${res.ctxDelta.getReassignments}"
          ))
          res
        }
        // 2.2. A partial function application:
        else {
          this.evalLogVerbose("8. Partial operator application detected (left-over formal parameters exist).")
          // Evaluate the body of the OperatorExpr with the modified context
          // which includes the reassignments from evaluating the operator
          // and the assignments from partial substitution of the parameters.
          this.evalLogVerbose("9. Evaluating body of operator...")
          val evaldBodyRes = body.eval(
            ctx :+ (evaldOpCtxDeltaOR ++ subCtxDeltaA)
          ).keepOnlyReassignments()
          this.evalLogVerbose("10. Evaluation of body complete.")
          // Return the evaluated body wrapped in an OperatorExpr with the
          // leftover parameters to indicate work still needs to be done to
          // complete the function application. Back-propagate all
          // reassignments made thus far.
          this.evalLogVerbose("11. Producing evaluation...")
          val res = Evaluation(
            OperatorExpr(leftOverParams, evaldBodyRes.evaldObj),
            evaldOpCtxDeltaOR ++ evaldBodyRes.ctxDelta
          )
          this.evalLogVerbose(Vector(
            "12.",
            "Evaluation produced.",
            s"Result from partial operator application: ${res.evaldObj}",
            s"Resultant context reassignments: ${res.ctxDelta.getReassignments}"
          ))
          res
        }
      }
      // 3. the structure cannot be used explicitly in a function application.
      case other => {
        this.evalLogVerbose("4. Structural assessment complete. Some other LIRO detected.")
        // Return the same ApplyExpr except with the operator evaluated.
        // (parameters remain unevaluated) //TODO
        // Back propagate the reassignments from the operator evaluation.
        this.evalLogVerbose("5. Producing evaluation...")
        val res = Evaluation(
          ApplyExpr(evaldOp, this.actualParams),
          evaldOpCtxDeltaOR
        )
        this.evalLogVerbose(Vector(
          "6.",
          "Evaluation produced.",
          s"Result from unapplyable operator: ${res.evaldObj}",
          s"Resultant context reassignments: ${res.ctxDelta.getReassignments}"
        ))
        res
      }
    }
  }

  @tailrec
  private def subInRec(
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object],
    ctxDelta    : ContextMutationSet = ContextMutationSet.empty
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
    ctx         : Context,
    formalParams: Vector[FormalParameter],
    actualParams: Vector[Object]
  ): (Boolean, Vector[FormalParameter]) = {
    if (formalParams.nonEmpty && actualParams.nonEmpty) {
      if (actualParams.head.checkType(ctx, formalParams.head.tpe)) {
        this.tryCheckAllRec(
          ctx.consolidatedWith(ContextMutationSet.empty.assign(
            formalParams.head.id,
            TypedObject(formalParams.head.tpe, actualParams.head)
          )
          ),
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

  def inferTheirTypes(
    ctx: Context,
    themToTheirMaybeTypes: Map[Identifier, Option[TypeIdentifier]]
  ): Map[Identifier, Option[TypeIdentifier]] = {
    // Assess the type of the operator
    this.op.inferType(ctx) match {
      // 1. Cannot determine the type of the operator,
      //    so no additional type info can be determined
      case None => themToTheirMaybeTypes
      // 2. Found some operator type
      case Some(OperatorType(formalParams, retTpe)) => {
        var _themToTheirMaybeTypes = themToTheirMaybeTypes
        // For each actualParam-formalParam pair,
        for ((ap, fp) <- this.actualParams.zip(formalParams.take(this.actualParams.length))) {
          // Cast the actual param to an identifier if possible
          ap match {
            // 2.1. Actual parameter is an identifier, so search it up in the map
            //      and see if its type has not yet been identified.
            case apAsIdentifier @ Identifier(_) => {
              _themToTheirMaybeTypes.get(apAsIdentifier) match {
                // 2.1.1. There is already a type associated with it, so check if
                //        it's the same as the type found in this operator's
                //        formal parameter.
                case Some(Some(tpe)) => {
                  // Types are not the same
                  if (tpe != fp.tpe) {
                    throw new Exception("Type inference is conflicting...") //TODO error handling
                  }
                }
                // 2.1.2. There is no type associated with it yet, so add this
                //        operator's formal parameter's type.
                case Some(None) => {
                  _themToTheirMaybeTypes = _themToTheirMaybeTypes + (apAsIdentifier -> Some(fp.tpe))
                }
                // 2.1.3. The actual parameter is not in the map, so it doesn't
                //        matter to this type inference
                case None => null
              }
            }
            // 2.2. Actual parameter is not an identifier,
            //      so recursively try to infer types on it
            case other => {
              _themToTheirMaybeTypes = other.inferTheirTypes(ctx, _themToTheirMaybeTypes)
            }
          }
        }
        _themToTheirMaybeTypes
      }
      case Some(other) => {
        throw new Exception("what the fuck man") //TODO error handling for bad type
      }
    }
  }

  def toRepr(indentLevel: Int): String = this.op match {
    case operatorIdent @ Identifier(name) => {
      org.cascas_project.cascas.lang.builtin.builtInCtx.get(operatorIdent) match {
        case Some(TypedObject(tpe, BuiltInExpr(_, _, _, _, _, Some(onApplyToRepr)))) => {
          onApplyToRepr(this, indentLevel)
        }
        case other => this.toReprImpl(indentLevel)
      }
    }
    case other => this.toReprImpl(indentLevel)
  }

  private def toReprImpl(indentLevel: Int): String = {
    this.op.toRepr(indentLevel) + "(" + this.actualParams.map(_.toRepr(indentLevel)).mkString(", ") + ")"
  }

  private def evalLogInfo(msg: String): Unit = {
    if (Logger.isLoggingActiveFor('INFO, 'LIRO)) {
      Logger.info('LIRO, "[ApplyExpr][Eval] " + msg)
    }
  }

  private def evalLogVerbose(msg: String): Unit = {
    if (Logger.isLoggingActiveFor('VERBOSE, 'LIRO)) {
      Logger.verbose('LIRO, "[ApplyExpr][Eval] " + msg)
    }
  }

  private def evalLogVerbose(msgs: Vector[String]): Unit = {
    if (Logger.isLoggingActiveFor('VERBOSE, 'LIRO)) {
      Logger.verbose('LIRO, "[ApplyExpr][Eval] " + msgs.mkString("\n    "))
    }
  }

}
