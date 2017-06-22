//=============================================================================
// ParseNode.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.Logger
import org.cascas_project.cascas.TopLevelInterpretable
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

class ParseTree(
  root: ParseNode
) extends TopLevelInterpretable {
  
  def interpretAsKind = root match {
    case NonTerminalNode('Statement, derivs) => {
      if (derivs(0).rep == 'Assign) {
        'AssignmentInterpretation
      }
      else if (derivs(0).rep == 'Expr) {
        'ExpressionInterpretation
      }
      else {
        val err = f"Invalid root node children contents:\n$root"
        Logger.error('PARSER, err)
        throw new Exception(err)
      }
    }
    case _ => {
      val err = f"Invalid root node:\n$root"
      Logger.error('PARSER, err)
      throw new Exception(err)
    }
  }

}
