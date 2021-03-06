//=============================================================================
// lang/TypedObject.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Object

//=============================================================================

case class TypedObject(tpe: TypeIdentifier, value: Object) extends ContextValue {

  override def toString(): String = f"$tpe -> $value"

}
