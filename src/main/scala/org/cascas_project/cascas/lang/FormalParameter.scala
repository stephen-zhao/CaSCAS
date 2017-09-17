//=============================================================================
// lang/FormalParameter.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Identifier

//=============================================================================

case class FormalParameter(id: Identifier, tpe: TypeIdentifier, manyness: Manyness = One)