//=============================================================================
// ParseNode.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Interpretable
import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.lang.EvaluatedTypeEnum
import org.cascas_project.cascas.tokens._

abstract class ParseNode {
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


sealed abstract class TypedParseNode 
    extends Interpretable {
  val rep: Symbol
  val typesAs: EvaluatedTypeEnum
}

final case class TypedNonTerminalNode(
  val rep: Symbol,
  val derivs: Vector[TypedParseNode],
  val typesAs: EvaluatedTypeEnum
) extends TypedParseNode

final case class TypedTerminalNode(
  val rep: Symbol,
  val token: Token,
  val typesAs: EvaluatedTypeEnum
) extends TypedParseNode
