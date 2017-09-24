//=============================================================================
// lang/liro/ListExpr.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.ContextMutationSet
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

case class ListExpr(list: Vector[Object]) extends Expr {

  def eval(ctx: Context): Evaluation = {
    // ListExpr behaves like a literal with respect to evaluations. Simply
    // return itself when doing an evaluation. Back propagate an empty
    // context mutation set.
    Evaluation(this, ContextMutationSet.empty)
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

  def toRepr(indentLevel: Int): String = {
    "[" + this.list.map(_.toRepr(indentLevel)).mkString(", ") + "]"
  }

  def :+(newElement: Object): ListExpr = ListExpr(this.list :+ newElement)

}

object ListExpr {

  def apply(): ListExpr = ListExpr(Vector())

}
