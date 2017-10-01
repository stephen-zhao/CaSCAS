//=============================================================================
// lang/liro/ListExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

import scala.annotation.tailrec

//=============================================================================

case class ListExpr(ofType: TypeIdentifier, list: Vector[Object]) extends Expr {

  def eval(ctx: Context): Evaluation = {
    // Evaluate each element in a ListExpr in order to evaluate a ListExpr.
    // Back propagate an empty context mutation set.
    this.evalRec(this.list.toList, ctx)
  }

  @tailrec
  private def evalRec(
    listToEval: List[Object],
    ctx:        Context,
    ctxDelta:   ContextMutationSet = ContextMutationSet.empty,
    accum:      List[Object] = List()
  ): Evaluation = {
    listToEval.headOption match {
      case None => Evaluation(ListExpr(this.ofType, accum.reverse.toVector), ctxDelta)
      case Some(obj) => obj.eval(ctx).keepOnlyReassignments() match {
        case Evaluation(evaldObj, evaldCtxDeltaOR) => {
          this.evalRec(listToEval.tail, ctx :+ evaldCtxDeltaOR, ctxDelta ++ evaldCtxDeltaOR, evaldObj :: accum)
        }
      }
    }
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    tpe match {
      case ApplyExpr(Identifier("List"), params) if params.length == 1 => {
        params(0) match {
          case listTypeParameter @ Identifier(_) => {
            this.list.forall(_.checkType(ctx, listTypeParameter))
          }
          case other => {
            false
          }
        }
      }
      case other => {
        false
      }
    }
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    if (list.nonEmpty) {
      val listTypes = this.list.map(_.inferType(ctx))
      if (listTypes.tail.forall(_ == listTypes.head)) {
        listTypes.head
      }
      else {
        //throw new Exception("List contains objects of more than one type")
        None
      }
    }
    else {
      //throw new Exception("Can't infer the type of an empty list expression")
      None
    }
  }

  def inferTheirTypes(
    ctx: Context,
    themToTheirMaybeTypes: Map[Identifier, Option[TypeIdentifier]]
  ): Map[Identifier, Option[TypeIdentifier]] = {
    var _themToTheirMaybeTypes = themToTheirMaybeTypes
    // Check if any of the direct elements are matching identifiers
    for (elem <- this.list) {
      elem match {
        // 1. Element is an identifier, so check to see if it matches
        //    anything in the map
        case elemAsIdentifier @ Identifier(_) => {
          _themToTheirMaybeTypes.get(elemAsIdentifier) match {
            // 1.1. Identifier matches, and identifier is already typed,
            //      so do nothing //TODO check type inference conflict
            case Some(Some(tpe)) => null
            // 1.2. Identifier matches, but identifier is not typed yet,
            //      so add type of whatever type the list is applied to
            case Some(None) => {
              this.inferType(ctx) match {
                case Some(ApplyExpr(Identifier("List"), Vector(appliedTpe))) => {
                  _themToTheirMaybeTypes = _themToTheirMaybeTypes +
                    (elemAsIdentifier -> Some(appliedTpe.asInstanceOf[TypeIdentifier]))
                }
                case other => null
              }
            }
            // 1.3. Identifier does not match, so leave it alone
            case None => null
          }
        }
        // 2. It is not an identifier, so leave it be
        case other => null
      }
    }
    _themToTheirMaybeTypes
  }

  def toRepr(indentLevel: Int): String = {
    "[" + this.list.map(_.toRepr(indentLevel)).mkString(", ") + "]"
  }

  def :+(newElement: Object): ListExpr = ListExpr(this.ofType, this.list :+ newElement)

}

object ListExpr {

  def apply(ofType: TypeIdentifier): ListExpr = ListExpr(ofType, Vector())

}
