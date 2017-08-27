package org.cascas_project.cascas.lang




package object builtin {

  private var builtInContextMutationSet: ContextMutationSet = ContextMutationSet.empty

  builtInContextMutationSet.assign(Identifier("+"), PlusOperator())

  //builtInContextMutationSet.assign(Identifier("-"), MultiplyOperator())

  //builtInContextMutationSet.assign(Identifier("...."), .....Operator())

  // e.g. builtInContextMutationSet.assign(Identifier("foo"), FooOperator)
  // Then, make a file within the BuiltIns directory named FooOperator.scala containing
  //
  // package org.cascas_project.cascas.lang.builtin
  //
  // object FooOperator extends BuiltInDefObj {
  //
  //   def onApply(ctx: Context): Evaluation = ............
  //
  //   def tpe = OperatorType( ... )
  //
  //   def formalParams = Vector[FormalParameter]( ... )
  //
  //   def returnTpe = .......
  //
  // }
  //

  val builtInCtx: Context = Context() :+ builtInContextMutationSet

  trait BuiltInDefObj {
    def onApply(ctx: Context): Evaluation
    def tpe: TypeIdentifier
    def formalParams: Vector[FormalParameter]
    def returnTpe: TypeIdentifier
    val obj: BuiltInExpr = BuiltInExpr(
      this.formalParams,
      this.onApply,
      this.returnTpe
    )
    def apply(): TypedObject = TypedObject(tpe, obj)
  }

}
