//=============================================================================
// lang/Evaluation.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Object

//=============================================================================

case class Evaluation(evaldObj: Object, ctxDelta: ContextMutationSet) {
  def keepOnlyReassignments(): Evaluation = {
    Evaluation(evaldObj, ctxDelta.onlyReassignments())
  }
}