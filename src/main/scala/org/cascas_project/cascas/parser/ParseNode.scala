//=============================================================================
// ParseNode.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.tokens._

trait ParseNodeLike {
  val rep: Symbol
}

abstract class ParseNode extends ParseNodeLike {
  val rep: Symbol
}

case class NonTerminalNode(
  val rep: Symbol,
  val derivs: Vector[ParseNode]
) extends ParseNode

case class TerminalNode(
  val rep: Symbol,
  val token: Token
) extends ParseNode
