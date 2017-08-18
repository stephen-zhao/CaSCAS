package org.cascas_project.cascas.lang

object Eval {
  def apply(o: Object, ctx: Context): Object = {
    o.eval(ctx)
  }
}
