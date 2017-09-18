//=============================================================================
// lang/builtin/package.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.{BuiltInExpr, Identifier}

//=============================================================================

package object builtin {

  private var builtInContextMutationSet: ContextMutationSet = ContextMutationSet.empty

  builtInContextMutationSet.assign(AdditionOperator.ident, AdditionOperator())
  builtInContextMutationSet.assign(MultiplyOperator.ident, MultiplyOperator())

  // e.g.
  //builtInContextMutationSet.assign(Identifier("...."), .....Operator())

  val builtInCtx: Context = Context() :+ builtInContextMutationSet

  trait BuiltInDefinition {
    private[builtin] def onApply(params : Map[String, Object], ctx: Context): Evaluation
    private[builtin] def tpe: TypeIdentifier
    private[builtin] def ident: Identifier
    private[builtin] def formalParams: Vector[FormalParameter]
    private[builtin] def returnTpe: TypeIdentifier
    private[builtin] def obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe,
      None
    )
    private[builtin] def apply(): TypedObject = TypedObject(tpe, obj)
  }

  trait BuiltInDefinitionWithCustomEval extends BuiltInDefinition {
    private[builtin] def onEval(ctx: Context): Evaluation
    private[builtin] override def obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe,
      Some(this.onEval)
    )
  }

}
