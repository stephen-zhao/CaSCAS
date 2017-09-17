//=============================================================================
// lang/Eval.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

object Eval {
  def apply(o: liro.Object, ctx: Context): liro.Object = {
    o.eval(ctx).evaldObj
  }
}
