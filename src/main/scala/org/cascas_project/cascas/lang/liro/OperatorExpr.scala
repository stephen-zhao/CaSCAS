//=============================================================================
// lang/liro/OperatorExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.OperatorType
import org.cascas_project.cascas.lang.TypeIdentifier

import scala.annotation.tailrec
import scala.collection.immutable.Queue

//=============================================================================

case class OperatorExpr(
  formalParams: Vector[FormalParameter],
  body:         Object
) extends Expr {

  def eval(ctx: Context): Evaluation = {
    // Evaluate the body of the Operator with respect to a context that contains
    // the formal parameters introduced, but unassigned.
    val evaldBody = this.body.eval(
      ctx.consolidatedWith(ContextMutationSet.empty.withIntroductions(this.formalParams))
    ).keepOnlyReassignments()
    // Return this OperatorExpr, but with its body simplified as done above, and
    // back propagate any reassignments that were done when evaluating the body.
    Evaluation(OperatorExpr(this.formalParams, evaldBody.evaldObj), evaldBody.ctxDelta)
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe match {
      case OperatorType(args, ret) => {
        if (this.formalParams.length == args.length) {
          if ((this.formalParams zip args).forall{ case (a, b) => a == b }) {
            this.body.checkType(
              ctx.consolidatedWith(ContextMutationSet.empty.withIntroductions(this.formalParams)),
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
    this.body.inferType(ctx :+ ContextMutationSet.empty.withIntroductions(this.formalParams)) match {
      case None => None
      case Some(bodyTpe) => {
        Some(OperatorType(this.formalParams, bodyTpe))
      }
    }
  }

//  @tailrec
//  private def inferTypeRec(
//    ctx:                 Context,
//    formalParams:        Queue[FormalParameter],
//    accum:               Map[],
//    noActionConsecCount: Int = 0
//  ): ContextMutationSet = {
//
//  }

  def inferTheirTypes(
    ctx: Context,
    themToTheirMaybeTypes: Map[Identifier, Option[TypeIdentifier]]
  ): Map[Identifier, Option[TypeIdentifier]] = {
    themToTheirMaybeTypes //TODO: make this actually work
  }

  def toRepr(indentLevel: Int): String = {
    "lambda(" + this.formalParams.map(fp => fp.id + " : " + fp.tpe).mkString(", ") + ")" + "(" + this.body.toRepr(indentLevel) + ")"
  }

}


