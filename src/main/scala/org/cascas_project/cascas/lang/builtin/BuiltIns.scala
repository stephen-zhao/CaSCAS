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
    
    def typeCheck (ctx : Context) : Option[Evaluation] = {
      var temp: Vector[Object] = Vector()
      this.onApply(typeCheckRec(this.formalParams, ctx, temp), ctx)
    }
    
    def typeCheckRec (fp : Vector[FormalParameter], ctx : Context, acc : Vector[Object]) : Vector[Object] = {
      ctx.get(fp.head.id) match {
        case Some(TypedObject(fp.head.tpe), v) => {
          v.eval(ctx).keepOnlyReassignments match {
            case Evaluation(l, ctxDelta) => {
              acc = acc :+ l
            }
            case Evaluation(other, ctxDelta) => {
              
            }
          }
          if (fp.tail.length() > 0) {
            typeCheckRec(fp.tail, ctx, acc)
          } else {
            acc
          }
        }
        
      case Some(TypedObject(other, _)) => {
        // report type mismatch error
        throw new Exception("type mismatch error") //TODO
      }

      // 3. case FAIL, is not assigned, then
      case Some(other) => {
        // report unassigned error
        throw new Exception("is not assigned") //TODO
      }
      
      // 4. case FAIL, is not defined, then
      case None => {
        // report undefined error
        throw new Exception("is not defined") //TODO
      }
      }
    }
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
