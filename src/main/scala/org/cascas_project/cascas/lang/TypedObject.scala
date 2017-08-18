package org.cascas_project.cascas.lang

case class TypedObject[T <: Object](tpe: TypeIdentifier, value: T) extends ContextValue {

  override def toString(): String = f"$tpe -> $value"

}
