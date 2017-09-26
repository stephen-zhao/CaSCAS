//=============================================================================
// lang/liro/BlockExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier
import scala.annotation.tailrec

//=============================================================================

case class BlockExpr(sequence: Vector[Object]) extends Expr {

  def eval(ctx: Context): Evaluation = {
    // To evaluate a block expression, evaluate the sequence of expressions
    // in order of definition, making sure to propagate changes in context
    // from one expression to the next.
    Logger.verbose('LIRO, "[BlockExpr][Eval] 1. Evaluating sequence recursively...")
    this.evalRecWithAllCtxDelta(this.sequence.toList, ctx)
  }

  @tailrec
  private def evalRecWithAllCtxDelta(
    seqToProcess: List[Object],
    ctx:          Context,
    ctxDelta:     ContextMutationSet = ContextMutationSet.empty,
    seqAccum:     List[Object] = List()
  ): Evaluation = {
    // Assess sequence structure
    seqToProcess.headOption match {
      // 1. There exists an object to be evaluated
      case Some(obj) => {
        // Evaluate the object with the current context
        Logger.verbose('LIRO, "[BlockExpr][Eval] 2*. Evaluating current object in block...")
        obj.eval(ctx) match {
          // Extract the evaluated object and changes to context
          case Evaluation(evaldObj, evaldObjCtxDelta) => {
            Logger.verbose('LIRO, "[BlockExpr][Eval] 3*. Evaluated.")
            // Recursively evaluate the remaining objects in the sequence,
            // making sure to update the context and context delta with the
            // new changes in context from the evaluation above. Also, update
            // the accumulator with the evaluated object.
            this.evalRecWithAllCtxDelta(
              seqToProcess.tail,
              ctx :+ evaldObjCtxDelta,
              ctxDelta ++ evaldObjCtxDelta,
              evaldObj :: seqAccum
            )
          }
        }
      }
      // 2. There are no more objects to evaluate
      case None => {
        // The resultant object is the evaluated form of the final object in
        // the sequence. The resultant change in context is the sum of all the
        // reassignments.
        Logger.verbose('LIRO, "[BlockExpr][Eval] 2*. No more objects in block; Producing evaluation...")
        val res = Evaluation(seqAccum.head, ctxDelta.onlyReassignments())
        Logger.verbose(
          'LIRO, "[BlockExpr][Eval] 3*.\n" +
                 "    Evaluation produced.\n" +
                s"    Result of evaluating the block: ${res.evaldObj}\n" +
                s"    Resultant context reassignments: ${res.ctxDelta.getReassignments}"
        )
        res
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

  def toRepr(indentLevel: Int): String = {
    val indentStringExterior = this.getIndentationString(indentLevel)
    val indentStringInterior = this.getIndentationString(indentLevel + 1)
    "\n" +
      indentStringExterior + "(\n" +
      indentStringInterior + this.sequence.map(_.toRepr(indentLevel + 1)).mkString(";\n" + indentStringInterior) + "\n" +
      indentStringExterior + ")"
  }

}
