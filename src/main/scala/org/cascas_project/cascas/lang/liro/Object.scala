//=============================================================================
// lang/liro/Object.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang.liro

//=============================================================================

import org.cascas_project.cascas.lang.Context
import org.cascas_project.cascas.lang.Evaluation
import org.cascas_project.cascas.lang.TypeIdentifier

//=============================================================================

trait Object {
  def eval(ctx: Context): Evaluation
  def checkType(ctx: Context, tpe: TypeIdentifier): Boolean
  def inferType(ctx: Context): Option[TypeIdentifier]
  def toRepr(identLevel: Int = 0): String
  protected def getIndentationString(indentLevel: Int): String = "  " * indentLevel
}
