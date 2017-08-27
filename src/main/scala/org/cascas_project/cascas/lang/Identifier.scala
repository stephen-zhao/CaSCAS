package org.cascas_project.cascas.lang

case class Identifier(name: String) extends TypeIdentifier with Object {

  override def toString(): String = name

  def eval(ctx: Context): Evaluation = {
    ctx.get(this) match {
      case Some(TypedObject(tpe, obj)) => obj.eval(ctx).keepOnlyReassignments
      case Some(tpe) => Evaluation(this, ContextMutationSet.empty)
      case None => Evaluation(this, ContextMutationSet.empty)
    }
  }

  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean = {
    ctx.get(this) match {
      case Some(TypedObject(chtpe, obj)) => chtpe == tpe
      case Some(chtpe: TypeIdentifier) => chtpe == tpe
      case None => throw new Exception("not in context") //TODO
    }
  }

  def inferType(ctx: Context): Option[TypeIdentifier] = {
    ctx.get(this) match {
      case Some(TypedObject(tpe, obj)) => Some(tpe)
      case Some(tpe: TypeIdentifier) => Some(tpe)
      case None => None
    }
  }

}

object Identifier {

  def apply(name: Symbol): Identifier = {
    Identifier(name.toString.drop(1))
  }

}

