package org.cascas_project.cascas.lang




package object builtin {

  private var builtInContextMutationSet: ContextMutationSet = ContextMutationSet.empty

  builtInContextMutationSet.assign(PlusOperator.ident, PlusOperator())

  // e.g.
  //builtInContextMutationSet.assign(MultiplyOperator.ident, MultiplyOperator())

  //builtInContextMutationSet.assign(Identifier("...."), .....Operator())

  val builtInCtx: Context = Context() :+ builtInContextMutationSet

  trait BuiltInDefinition {
    def onApply(ctx: Context): Evaluation
    def tpe: TypeIdentifier
    def ident: Identifier
    def formalParams: Vector[FormalParameter]
    def returnTpe: TypeIdentifier
    def obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe,
      None
    )
    def apply(): TypedObject = TypedObject(tpe, obj)
  }

  trait BuiltInDefinitionWithCustomEval extends BuiltInDefinition {
    def onEval(ctx: Context): Evaluation
    override def obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe,
      Some(this.onEval)
    )
  }

}
