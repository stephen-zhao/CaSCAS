package org.cascas_project.cascas.lang

trait Object {
  def eval(ctx: Context): Evaluation
  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean
  def inferType(ctx: Context): Option[TypeIdentifier]
}
