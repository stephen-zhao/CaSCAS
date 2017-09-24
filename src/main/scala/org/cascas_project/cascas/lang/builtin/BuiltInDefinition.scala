//=============================================================================
// lang/builtin/BuiltInDefinition.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.builtin

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.FormalParameter
import org.cascas_project.cascas.lang.OperatorType
import org.cascas_project.cascas.lang.TypeIdentifier
import org.cascas_project.cascas.lang.TypedObject
import org.cascas_project.cascas.lang.liro.ApplyExpr
import org.cascas_project.cascas.lang.liro.BuiltInExpr
import org.cascas_project.cascas.lang.liro.Identifier
import org.cascas_project.cascas.lang.liro.ListExpr
import org.cascas_project.cascas.lang.liro.Object

//=============================================================================

trait BuiltInDefinition {

  // The following methods are abstract and need implementations per BuiltIn
  protected def onApply(params: Map[String, Object], ctx: Context): Object

  protected def ident: Identifier

  protected def formalParams: Vector[FormalParameter]

  protected def returnTpe: TypeIdentifier

  // To override the onApplyToRepr method, mix-in an AppearsAs trait
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String]

  // The following methods are defined and "private" to BuiltInDefinition and
  // customized sub-traits
  protected final def tpe: TypeIdentifier = OperatorType(this.formalParams)(this.returnTpe)
  protected def obj: BuiltInExpr = BuiltInExpr(
    this.ident.name,
    this.formalParams,
    this.onApply,
    this.returnTpe,
    None,
    this.maybeOnApplyToRepr
  )
  private[builtin] def apply(): TypedObject = TypedObject(tpe, obj)
}

trait BuiltInDefinitionWithCustomEval extends BuiltInDefinition {
  protected def onEval(ctx: Context): Evaluation
  protected override def obj: BuiltInExpr = BuiltInExpr(
    this.ident.name,
    this.formalParams,
    this.onApply,
    this.returnTpe,
    Some(this.onEval),
    this.maybeOnApplyToRepr
  )
}

//TODO: This is temporary. There should not be any unappliable operators
trait Unapplyable {
  protected def onApply(params : Map[String, Object], ctx: Context): Object = {
    throw new Exception("This operator is unappliable") //TODO
  }
}

trait BuiltInOnApplyAppearance

trait AppearsAsStandardOp extends BuiltInOnApplyAppearance {
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String] = None
}

trait AppearsAsBinaryInfixOp extends BuiltInOnApplyAppearance {
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String] = Some((
    application: ApplyExpr,
    indentLevel: Int
  ) => {
    if (application.actualParams.length == 0) {
      application.op.toRepr(indentLevel) + "()"
    }
    else if (application.actualParams.length == 1) {
      application.actualParams.head match {
        case ListExpr(actualParamsList) => {
          actualParamsList.map(_.toRepr(indentLevel)).mkString(" " + application.op.toRepr(indentLevel) + " ")
        }
        case other => {
          application.op.toRepr(indentLevel) + "(" + other.toRepr(indentLevel) + ")"
        }
      }
    }
    else {
      application.actualParams.map(_.toRepr(indentLevel)).mkString(" " + application.op.toRepr(indentLevel) + " ")
    }
  })
}

trait AppearsAsUnaryPrefixOp extends BuiltInOnApplyAppearance {
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String] = Some((
    application: ApplyExpr,
      indentLevel: Int
  ) => {
    application.op.toRepr(indentLevel) + application.actualParams.map(_.toRepr(indentLevel)).mkString(
      "<<this should not show, using unary prefix appearance on non-unary operator>>"
    )
  })
}

trait AppearsAsUnaryPostfixOp extends BuiltInOnApplyAppearance {
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String] = Some((
    application: ApplyExpr,
    indentLevel: Int
  ) => {
    application.actualParams.map(_.toRepr(indentLevel)).mkString(
      "<<this should not show, using unary prefix appearance on non-unary operator>>"
    ) + application.op.toRepr(indentLevel)
  })
}

trait AppearsAsCustom extends BuiltInOnApplyAppearance {
  protected def maybeOnApplyToRepr: Option[(ApplyExpr, Int) => String] = Some(this.customOnApplyToRepr)
  protected def customOnApplyToRepr(application: ApplyExpr, indentLevel: Int): String
}
