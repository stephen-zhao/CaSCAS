//=============================================================================
// lang/builtin/package.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.{BuiltInExpr, Identifier, Object}

//=============================================================================

package object builtin {

  private var builtInContextMutationSet: ContextMutationSet = ContextMutationSet.empty

  builtInContextMutationSet.introduce(Identifier("Number"),Identifier("Type"))
  builtInContextMutationSet.introduce(Identifier("Bool"),Identifier("Type"))

  builtInContextMutationSet.assign(ListOperator.ident, ListOperator())

  builtInContextMutationSet.assign(AdditionOperator.ident, AdditionOperator())
  builtInContextMutationSet.assign(MultiplyOperator.ident, MultiplyOperator())

  // e.g.
  //builtInContextMutationSet.assign(Identifier("...."), .....Operator())

  val builtInCtx: Context = Context() :+ builtInContextMutationSet

  trait BuiltInDefinition {
    private[builtin] def onApply(params : Map[String, Object], ctx: Context): Object
    private[builtin] def ident: Identifier
    private[builtin] def formalParams: Vector[FormalParameter]
    private[builtin] def returnTpe: TypeIdentifier
    private[builtin] def tpe: TypeIdentifier = OperatorType(this.formalParams)(this.returnTpe)
    private[builtin] def obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe,
      None
    )
    private[builtin] def apply(): TypedObject = TypedObject(tpe, obj)
  }

  //TODO: This is temporary. There should not be any unappliable operators
  trait Unapplyable {
    private[builtin] def onApply(params : Map[String, Object], ctx: Context): Object = {
      throw new Exception("This operator is unappliable") //TODO
    }
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
