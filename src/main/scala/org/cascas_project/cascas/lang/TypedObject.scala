package org.cascas_project.cascas.lang

case class TypedObject(tpe: TypeIdentifier, value: Object) extends ContextValue {

  override def toString(): String = f"$tpe -> $value"

}
