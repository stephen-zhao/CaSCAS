//=============================================================================
// ParseNode.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.lang.EvaluatedType._
import org.cascas_project.cascas.tokens._

sealed abstract class ParseNode(kind: Symbol) {
  
  // Protected field to represent type. When parsed, the node will not have
  // a type. The analyzer will assign it a type during that later stage.
  protected var etype: EvaluatedTypeEnum = UnTyped
  def giveType(t: EvaluatedTypeEnum): Unit = { this.etype = t }
  def typesAs(): EvaluatedTypeEnum = { this.etype }

  protected var kindImpl: Symbol = kind
  def getKind(): Symbol = { this.kindImpl }

}

final case class TerminalNode(
  val token: Token
)(kind: Symbol) extends ParseNode(kind) {

}

sealed abstract class NonTerminalNode(kind: Symbol) extends ParseNode(kind) {

}

final case class UnaryOperatorNode(
  val operator: ParseNode,
  val operand: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class BinaryOperatorNode(
  val operator: ParseNode,
  val operands: (ParseNode, ParseNode)
)(kind: Symbol) extends NonTerminalNode(kind)

final case class AssignNode(
  val assignee: ParseNode,
  val assigned: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class ReAssignNode(
  val assignee: ParseNode,
  val assigned: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class OperatorAssignNode(
  val assignee: ParseNode,
  val params:   ParseNode,
  val assigned: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class WrapperNode(
  val wrapped: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class StatementsNode(
  val statements: Vector[ParseNode]
)(kind: Symbol) extends NonTerminalNode(kind)

final case class LambdaNode(
  val params: ParseNode,
  val body:   ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class SequenceNode(
  val sequence: Vector[ParseNode]
)(kind: Symbol) extends NonTerminalNode(kind)

final case class WhileNode(
  val predicate: ParseNode,
  val body:      ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class ForNode(
  val iterator:   ParseNode,
  val collection: ParseNode,
  val body:       ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)

final case class IfNode(
  val test:      ParseNode,
  val whenTrue:  ParseNode,
  val whenFalse: ParseNode
)(kind: Symbol) extends NonTerminalNode(kind)
