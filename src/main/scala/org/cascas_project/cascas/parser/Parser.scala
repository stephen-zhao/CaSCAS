//=============================================================================
// Parser.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.parser

import org.cascas_project.cascas.tokens._
import org.cascas_project.cascas.parser._

class Parser {

  val lrm: LRMachine = new LRMachineGenerator().generate

  def parse(tokens: Seq[Token]): ParseNode = {
    lrm.rightmostDerive(tokens) match {
      case None => {
        // TODO: placeholder for type-checkiness
        new TerminalNode('EMPTY, WhitespaceToken(" "))
      }
      case Some(tree) => {
        tree
      }
    }
  }

  def withoutUnparseables(tokens: Vector[Token]): Vector[Token] = {
    tokens.filter(t => t match { 
      case _: WhitespaceToken   => false
      case _: CommentToken      => false
      case _                    => true
    })
  }


}
